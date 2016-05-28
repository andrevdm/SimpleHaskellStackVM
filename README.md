# Creating a Stack Machine with Haskell

## About the Project

This is a small demonstration project showing how a simple byte code interpreting stack machine (virtual machine) can be built with Haskell. It is not a production VM nor of any particular practical use but is rather a simple demonstration of how a stack machine can be built.

I built this for mainly as a project for learning Haskell, i.e. something a little bigger to work on. So NB this is probably not idiomatic Haskell, and may have some newbie mistakes. Hopefully it is interesting enough despite this... 

### Stack Machines

> A stack machine is a real or emulated computer that uses a pushdown stack rather than individual machine registers to evaluate each sub-expression in the program. A stack computer is programmed with a reverse Polish notation instruction set. - [wikipedia \[1\]](https://en.wikipedia.org/wiki/Stack_machine)

Stack machines are simpler to implement than [register machines](https://en.wikipedia.org/wiki/Register_machine) but are still practical even for production VMs.

### Writing the VM in Haskell

Virtual machines are typically written in a low level language like C for maximum efficiency. They also are typically written using mutable data structures. Here I'm using Haskell and pure functional data structures. I have absolutely no performance goals, so there are no constraints I need to worry about.

A few design decisions

1. I'm using a Data.Sequence rather than a list. While I'm not concerned about performance using a linked list to do random access is still a bad idea

2. Try to avoid bottom (⊥), so use "safe" versions whenever ⊥ could otherwise be returned (toEnum, index, head etc)

3. I'm using [Stephen Diehl's Protolude \[2\]](https://github.com/sdiehl/protolude) and removing the default prelude 

Using the immutable data structures like Data.Sequence had worked out nicely and means that as the VM runs you can keep a full history of the VM's state. So you have everything (apart from time) you need to build a nice visualiser / 'time travelling' debugger.

## The VM

### Opcodes & byte code

The opcodes are the operations that the virtual CPU can perform. These are simple operations like push, pop, add and jump. The opcodes are encoded as bytes together with the opcode parameters (e.g. the value to push) this forms the byte code.

Here is the current set of opcodes understood by the CPU

```haskell
data Operation = Nop       -- No operation
               | Halt      -- Stop CPU execution
               | Push      -- Push a value onto the stack
               | Pop       -- Pop the most recent value from the stack
               | PopPrev   -- Pop n values before the most recent value from the stack
               | Add       -- Add the top two items on the stack
               | Inc       -- Increment the top item on the stack
               | Dup       -- Duplicate the most recent item on the stack
               | Jmp       -- Jump unconditionally to a location
               | Bne       -- Pop the top two items, compare and branch if the values are not equal
               | Beq       -- Pop the top two items, compare and branch if the values are equal
               | Bgt       -- Pop the top two items, compare and branch if value 1 is greater than value 2
               | Bgte      -- Pop the top two items, compare and branch if value 1 is greater or equal to value 2
               | Blt       -- Pop the top two items, compare and branch if value 1 is less than value 2
               | Blte      -- Pop the top two items, compare and branch if value 1 is less than or equal to value 2
               | Call      -- Call a function
               | Ret       -- Return from a function
               | LdArg     -- Push local value n onto the stack
               deriving (Show, Eq, Ord, Enum, Bounded)
```

### The virtual CPU

The data structure representing the CPU is defined below

```haskell
data Cpu = Cpu { ip :: Int               -- Instruction pointer
               , fp :: Int               -- Frame pointer
               , cpuStack :: S.Seq Int   -- The stack
               , cpuGlobals :: S.Seq Int -- Gloal variables
               , ranOp :: Int            -- The last opcode that was executed
               , state :: Text           -- Debugging message
               , debug :: Bool           -- Enable/disable debugging
               , panic :: Bool           -- Is the CPU in a faulted state
               }
         deriving (Show, Eq)

-- | Default empty/initial CPU state
emptyCpu :: Cpu
emptyCpu = Cpu { ip = -1
               , fp = -1
               , cpuStack = S.empty
               , cpuGlobals = S.empty
               , state = ""
               , debug = True
               , ranOp = 0
               , panic = False 
               }
```

This is fairly minimal but it’s more than enough for a simple VM like this. 

Some things to note
* The stack (cpuStack) is part of the CPU. This makes sense for a stack machine since a stack is core to everything it does. It also means that as the CPU runs you get a full history of each stack state along with the CPU flags at the time each opcode was run
* There is no need for a stack pointer since the stack is a 'dynamically' growing Data.Sequence. I.e. sp always points to the head of cpuStack.
* The instruction pointer (ip) points to the next instruction to run. 
* In this implementation the byte stream is fixed (no self-modifying code), so there is no need to copy it on each CPU operation
* The frame pointer (fp) is discussed below in the section about function calls (Call & Ret)

### The byte code assembler

Rather than writing the byte code in hex a very simple assembler is used. Later on additional assemblers and compliers can be layer on top of this low level assembler which does little more than convert opcode mnemonics to byte code.

An operation can take parameters from the byte code stream. For instance a ***push*** instruction takes a parameter that is the value to push onto the stack. The type that represents this is the inventively named ***OpAndParam***

```haskell
-- | A single CPU operator and its parameters
type OpAndParam = (Operation, [Int])
```

Having a type that defines the number of parameters an op takes and how many values it pops off the stack allows the assembler to perform some basic checks. The interpreter can also use this definition when running the code.

```haskell
-- | Configuration for an operation
-- | opParamCount = number of paramaters taken from the code stream
-- | opPopsCount = number of values this op pops from the stack
-- | opSimple = determines if the op needs full access to cpu state to change things like the fp and ip
-- |            note that 'complex' instructions do not need to honour opParamCount and opPopsCount
-- |            e.g. a 'ret' instruction pops a variable number of parameters
data Instruction = Instruction { opCode :: Operation
                               , opPopsCount :: Int
                               , opParamCount :: Int
                               , opSimple :: Bool
                               }
                 deriving (Show, Eq)
```

The instructions can then be setup and a map created from opcode to Instruction

```haskell
-- | Config for the op codes
instructions :: [Instruction]
instructions = [ Instruction { opCode = Nop, opParamCount = 0, opPopsCount = 0, opSimple = True }
                -- ...
               , Instruction { opCode = Call, opParamCount = 1, opPopsCount = 0, opSimple = False }
               ]

-- | Instructions indexed by opcode
instrByOp :: Map.Map Operation Instruction
instrByOp = Map.fromList $ map (\i -> (opCode i, i)) instructions
```

The assembler then does nothing more than converting the opcode enum to a byte (an Int in the code but it would be serialised as a byte) checking the number of parameters for each opcode. It is small enough to be pasted in full here

```haskell
-- | A single assembler error
data AssemblerError = AssemblerError Integer Operation Text deriving (Show, Eq)

-- | Compiles the list to byte code
-- | Returns as many errors as possible rather than just first error
assembleByteCode :: [(Operation, [Int])] -> Either [AssemblerError] [Int]	
assembleByteCode code =
  let res = foldl assemble [] code in
  case lefts res of
    [] -> Right $ concat $ rights res
    errors -> Left errors
  
  where
    assemble :: [Either AssemblerError [Int]] -> OpAndParam -> [Either AssemblerError [Int]]
    assemble res (op, prms) =
      res ++ case Map.lookup op instrByOp of
               Nothing -> [Left $ AssemblerError (toInteger $ length res) op "unknown op code"]
               Just i ->
                 if opParamCount i == length prms
                 then [Right $ fromEnum (opCode i) : prms]
                 else [Left $ AssemblerError (toInteger $ length res) op "incorrect number of parameters"]
```

### The interpreter

With all of that in place the interpreter can finally be written. 

```haskell
-- | Interpreter for the byte code
-- | Given a byte code stream will 'run' the code
-- | If debug is enabled then the full history (all states) will be returned. 
interpretByteCode :: S.Seq Int -> [Cpu]
```

The interpreter takes the output of the assembler or bytes loaded from a file and runs it producing CPU state along the way.  In debug mode the interpreter stores all the states as it interprets, in "non-debug" mode only the last state is kept. _Note that currently the interpreter is always in debug mode_

Before looking at the implementation of the interpreter its worth going over a few examples of how it should operate

##### Push & Pop

```text
                                                  +--------+
                          +--------+              |  123   |
   +--------+             |  123   |              |  456   |
   +--------+    push 123 +--------+    push 456  +--------+
```

In the first example the value 123 is pushed onto an empty stack. Then the value 456 is pushed. The head of the stack is at the "bottom"

```text
  +--------+             
  |  123   |        +--------+
  |  456   |        |  123   |
  +--------+    pop +--------+
```

A ***pop*** is the opposite of a ***push***. The pop operation get the most recent value from the stack (FIFO) in this case 456 leaving 123 on the stack.


##### Add

An ***add*** operation pops the top two items from the stack, adds them and pushes the result back onto the stack

```text
           +--------+             
           |  100   |        +--------+
push 100   |  123   |        |  223   |
push 123   +--------+    add +--------+
```

Look at the definition of the instructions for these three operators

```haskell
Instruction { opCode = Push, opParamCount = 1, opPopsCount = 0, opSimple = True }
Instruction { opCode = Pop, opParamCount = 0, opPopsCount = 1, opSimple = True }
Instruction { opCode = Add, opParamCount = 0, opPopsCount = 2, opSimple = True }
```

From this you can see that the ***Instruction*** shows that a ***push*** takes one parameter, a ***pop*** takes no parameters but pops a single value off the stack and an ***Add*** pops two values off the stack. As noted in the code comments ***opSimple*** indicates that these are simple operators with fixed stack effects.

##### Jmp

The ***Jmp*** operator performs an unconditional jump to a fixed location relative to the start of the code stream. I.e. ***Jmp 100*** sets the instruction pointer to 100 and execution continues from there.

Consider the following simple list of ops in Haskel

```haskell
 [ (Jmp, [3])
 , (Nop, [])
 , (Halt, [])
 ]
```

This gets assembled into the following byte code
```text
00: 0903  -- Jmp 3
02: 00    -- Nop
03: 02    -- Halt
```

The Jmp instruction causes the CPU to set the instruction pointer (ip) to 3. In this example that means that the ***Nop*** at offset 2 is skipped and execution continues with the ***Halt*** operation at offset 3

##### Branching (Beq, Bne, Blr, Blte, Bgt, Bgte)

Branching is a conditional jump. The top two values are popped off the stack compared based on the type of conditional operator.  

In the following example the values 1 and to are pushed. ***Bgt*** is executed and if 2 is greater than 1 then the CPU jumps to location 7. If no then it continues executing at the location after the branch (6)

```haskell
 [ (Push, [1])
 , (Push, [2])
 , (Bgt, [7])
 , (Nop, [])
 , (Halt, [])
 ]
```

```text
00: 0301  -- Push 1
02: 0302  -- Push 2
04: 0C07  -- Bgt 7
06: 00    -- Nop
07: 02    -- Halt
```

```text
         +--------+             
         |   1    |        +--------+
push 1   |   2    |        |        |
push 2   +--------+    Bgt +--------+
```

This is what the history of the CPU would look like for the above example

```haskell
Cpu {ip = -1, cpuStack =    [], ranOp = 0,  state = "",     panic = False}
Cpu {ip =  1, cpuStack =   [1], ranOp = 3,  state = "Push", panic = False}
Cpu {ip =  3, cpuStack = [2,1], ranOp = 3,  state = "Push", panic = False}
Cpu {ip =  5, cpuStack =    [], ranOp = 12, state = "Bgt",  panic = False}
Cpu {ip =  6, cpuStack =    [], ranOp = 0,  state = "Nop",  panic = False}
Cpu {ip =  7, cpuStack =    [], ranOp = 2,  state = "Halt", panic = True}
```

##### Call & Ret

A function call is much like a jmp except that you have to store an address to return to. You could have two stacks, one for values and one for return address. It’s more common however to have a single stack with "stack frames". 

As a trivial example consider a the case when there are no parameters

```text
00: 1003  -- Call 03
02: 02    -- Halt
03: 00    -- Nop
04: 11    -- Ret
```

The CPU does the following
* Executes the ***call*** operation and pushes the return address onto the stack, i.e. the next instruction after the ***call***. Here it is the  ***halt*** at 02.
* The ip is set to 03, the offset of the function, and the CPU executes the function (***nop***)
* The ***ret*** operation gets the return address (02) from the stack and updates the ip
* The ***halt*** at 02 is executed.

```text
         +--------+        
         |   02   |        +--------+
Call 03  +--------+    Ret +--------+
```

However this simple scheme does not work when you have variable numbers of parameters, locals etc. This is where the frame pointer (fp) and stack frames come in.

A stack frame is the set of data stored on the stack for each method call. In this virtual machine that is
 1. The return address
 2. Parameters for the function
 3. The previous frame pointer value


As an example consider a function that adds two numbers and returns the sum.

* The caller pushes the two values to be added (123 and 22), these are pushed in reverse order. I.e. parameter 1 last.

```text
   00: 0322  -- Push 22    +--------+ ip = 4
__ 02: 037b  -- Push 123   |   22   | fp = -1
   04: 1003  -- Call 09    |  123   |
   06: 0502  -- PopPrev 2  +--------+
   08: 02    -- Halt   
   09: 1202  -- LdArd 2
   0b: 1201  -- LdArg 1
   0c: 06    -- Add    
   0d: 11    -- Ret
```

* The current frame pointer (-1) is stored. If no function has been called this will be -1. Further down in the example this will make more sense
* The return address (06) is pushed onto the stack
* The frame pointer is set to the start of the stack frame (02), i.e. position 2 from the end of the stack
* The CPU sets the instruction pointer and "jumps" to the function (09) being called. 

```text
   00: 0322  -- Push 22    +--------+ ip = 0b
   02: 037b  -- Push 123   |   22   | fp = 2
   04: 1003  -- Call 09    |  123   |
   06: 0502  -- PopPrev 2  |   -1   |
   08: 02    -- Halt       |    6   | <-- fp
   09: 1202  -- LdArd 2    |   22   | 
__ 0b: 1201  -- LdArg 1    |  123   |
   0c: 06    -- Add        +--------+
   0d: 11    -- Ret
```

* The function can then push any values it needs onto the stack and do its work.
* Here the function loads the two arguments ***LdArg 1*** and ***LdArg 2*** (i.e. get params to top of stack) and then calls ***Add*** to add them
* In this VM a function always returns a single value on the stack

```text
   00: 0322  -- Push 22    +--------+ ip = 0c
   02: 037b  -- Push 123   |   22   | fp = 2
   04: 1003  -- Call 09    |  123   |
   06: 0502  -- PopPrev 2  |   -1   |
   08: 02    -- Halt       |    6   | <-- fp
   09: 1202  -- LdArd 2    |  145   | 
   0b: 1201  -- LdArg 1    +--------+
__ 0c: 06    -- Add        
   0d: 11    -- Ret
```

* Then when a ***ret*** operation is executed the CPU needs to do the reverse.
* The return value is popped, and the stack shifted back to the fp
* Notice that the original parameters are still on the stack. This is normal for the cdecl calling convention. The caller is responsible for cleaning up.

```text
   00: 0322  -- Push 22    +--------+ ip = 06
   02: 037b  -- Push 123   |   22   | fp = -1
   04: 1003  -- Call 09    |  123   |
   06: 0502  -- PopPrev 2  |  145   | 
   08: 02    -- Halt       +--------+
   09: 1202  -- LdArd 2    
   0b: 1201  -- LdArg 1    
   0c: 06    -- Add        
__ 0d: 11    -- Ret
```

* The ***PopPrev*** operation is used to do the parameter clean-up by the caller. It pops the number of items specified before the item at the top of the stack

```text
   00: 0322  -- Push 22    +--------+ ip = 08
   02: 037b  -- Push 123   |   145  | fp = -1
   04: 1003  -- Call 09    +--------+
__ 06: 0502  -- PopPrev 2  
   08: 02    -- Halt       
   09: 1202  -- LdArd 2    
   0b: 1201  -- LdArg 1    
   0c: 06    -- Add        
   0d: 11    -- Ret
```

The important thing to notice is that since the old frame pointer is stored on the stack you are able to call multiple functions and always be able to return to the previous function. Also having the fp lets you unwind the stack to the frame no matter how many items the current function may have pushed onto the stack, i.e. you don't need to try and track that	

Here is the output for the code above

```
{ip = -1, fp = -1, cpuStack = [],                   ranOp =  0, state = "",        panic = False}
{ip =  1, fp = -1, cpuStack = [22],                 ranOp =  3, state = "Push",    panic = False}
{ip =  3, fp = -1, cpuStack = [123,22],             ranOp =  3, state = "Push",    panic = False}
{ip =  8, fp =  2, cpuStack = [6,-1,123,22],        ranOp = 16, state = "Call",    panic = False}
{ip = 10, fp =  2, cpuStack = [22,6,-1,123,22],     ranOp = 18, state = "LdArg",   panic = False}
{ip = 12, fp =  2, cpuStack = [123,22,6,-1,123,22], ranOp = 18, state = "LdArg",   panic = False}
{ip = 13, fp =  2, cpuStack = [145,6,-1,123,22],    ranOp =  6, state = "Add",     panic = False}
{ip =  5, fp = -1, cpuStack = [145,123,22],         ranOp = 17, state = "Ret",     panic = False}
{ip =  7, fp = -1, cpuStack = [145],                ranOp =  5, state = "PopPrev", panic = False}
{ip =  8, fp = -1, cpuStack = [145],                ranOp =  2, state = "Halt",    panic = True}
```

##### The interpreter code

```haskell
-- | Interpreter for the byte code
-- | Given a byte code stream will run the code
interpretByteCode :: S.Seq Int -> [Cpu]
```

* The interpreter converts a sequence of byte codes into a list of CPUs. Final CPU is the final state


```haskell
  interpret [emptyCpu] byteCode
  where
    interpret :: [Cpu] -> S.Seq Int -> [Cpu]

    -- Ensure that this function is not called with an empty CPU list
    interpret [] _ = [emptyCpu { state = "INVALID: no start CPU" }]

    -- Start interpreting 
    interpret cpus@(cpu:_) code =
      -- Move to next op code
      let atIp = ip cpu + 1 in

      -- Try get the code byte at index atIp 
      case indexMay code atIp of
        Nothing ->
          -- No byte at expected index, return error
          cpu { ip = atIp, ranOp = 0, state = "INVALID: invalid ip index, reading past end of byte stream", panic = True } : cpus
          
        Just opByte ->
          -- Ensure this is a valid opcode

          case toEnumMay opByte :: Maybe Operation of
            Nothing ->
              -- This is not a valid opcode, error
              cpu { ip = atIp, ranOp = opByte, state = "INVALID: Unknown op", panic = True } : cpus

            Just op ->
              -- Get the instruction for the op code
              case (op, Map.lookup op instrByOp) of
                (_, Nothing) ->
                  -- The byte was an opcode enum but was not configured as an instruction
                  cpu { ip = atIp, ranOp = opByte, state = "INVALID: Op not found", panic = True } : cpus
                  
```
* Start interpreting with an empty CPU
* The code does the following checks, to get a valid instruction for the byte
  1. Check that there is a start CPU
  1. Check that there is an opcode in the sequence at the ip index
  1. Check that the opcode is valid, i.e. belongs to the ***Operation*** enum
  1. Check that the opcode is in the instruction map ***instrByOp***


```haskell
                (_, Just instr) ->
                  -- 'params' are the bytes from the code stream that are used as parameters for the op, e.g. to be pushed onto stack
                  -- get the params from the byte stream into a list
                  let paramsCount = opParamCount instr in 
                  let params = S.take paramsCount $ S.drop (atIp + 1) code in

                  -- 'Pops' are the bytes popped from the stack and used by the current instruction
                  -- get the values from the stack into the list
                  let popsCount = opPopsCount instr in 
                  let (pops, remainingStack) = S.splitAt popsCount $ cpuStack cpu in

                  if S.length params > paramsCount 
                  then
                    cpu { ip = atIp, ranOp = opByte, state = "Code underflow", panic = True } : cpus
                  else
                    if length pops < popsCount
                    then cpu { ip = atIp, ranOp = opByte, state = "Stack underflow", panic = True } : cpus
```
* Get the parameter bytes from the code stream. E.g. Push takes a single param, the value to be pushed onto the stack
* Pop the required number of values from the stack. 
* Check that both operations succeed


```haskell
                    else
                      -- Interpret the opcode using the simple/complex interpreter as indicated by the instruction
                      let next = if opSimple instr then
                                   let res = interpretSimple emptyCpu { ip = atIp + paramsCount, state = show op } op (toList pops) params in
                                   cpu { ranOp = opByte
                                       , cpuStack = cpuStack res <> remainingStack
                                       , ip = ip res
                                       , panic = panic res
                                       , state = state res
                                       }
                                 else
                                   interpretComplex cpu { ip = atIp + paramsCount, state = show op, ranOp = opByte } op (toList pops) params remainingStack
                                  
                      in if panic next
                      then
                        -- In panic state, add current CPU and stop executing
                        next : cpus
                      else
                        -- Everything fine, add CPU and interpret nex byte
                        interpret (next : cpus) code

```

Finally the core interpreter code can be called. Since the params, pops are now stored as lists and all checks performed this code is quite simple.

* Remember that in this VM there are two types of ***Instructions***; simple and complex. Simple instructions are fully defined by the ***Instruction***. Complex instructions have full control over the CPU
* Simple instructions are given an empty CPU and return a CPU with the values that need to be changed. For example simple instructions can not pop extra values or change the fp
* Complex instructions are not fully defined by the ***Instruction*** and can change the CPU in any way they need to.


##### Simple instructions

```haskell
    -- | Simple instructions, can not directly change CPU state, e.g. cant set ip/fp and they just return data to be added to stack
    interpretSimple :: Cpu -> Operation -> [Int] -> S.Seq Int -> Cpu
    interpretSimple cpu op popped params =
      case op of
        Nop -> cpu 
```

* Nop just returns the current CPU

```haskell
        Push -> cpu { cpuStack = params }
```

* Push sets the stack (appended with current stack in the interpreter code above)

```haskell
        Pop -> cpu
```

* Pop also just returns the CPU. This is because as a "simple" instruction the fact that it pops a single value is configured in the ***Instruction*** and the interpreter will do that.


```haskell
        Bne -> branchIf cpu popped params (/=)
        Beq -> branchIf cpu popped params (==)
        Bgt -> branchIf cpu popped params (>)
        Bgte -> branchIf cpu popped params (>=)
        Blt -> branchIf cpu popped params (<)
        Blte -> branchIf cpu popped params (<=)
```

* The branch instructions call the ***branchIf*** function with the appropriate conditional operator


```haskell
    -- | Perform a branch instruction if the predicate for the op (beq, bne, bgte etc) is true
    branchIf :: Cpu -> [Int] -> S.Seq Int -> (Int -> Int -> Bool) -> Cpu
    branchIf cpu popped params prd =
      case head params of
        Nothing -> cpu { panic = True, state = "missing param" }
        Just jmp ->
          case popped of
            [b, a] -> cpu { ip = if prd a b then ip cpu + jmp else ip cpu } 
            _ -> cpu { panic = True, state = "invalid stack" }
```

* The branch checks the predicate (prd) and if it is True the ip is updated to the ip specified by the branch.


##### Complex Instructions

```haskell
    -- | Complex instructions have full access to the CPU and must ensure the CPU is correctly setup for the next instruction
    interpretComplex :: Cpu -> Operation -> [Int] -> S.Seq Int -> S.Seq Int -> Cpu
    interpretComplex cpu op popped params remainingStack =
      case op of
```

```haskell
        Call ->
          -- Set up a stack frame and jump to the call location. See the comments above for details of the stack frame
          singleVal cpu (toList params) (\v -> cpu { ip = v - 1 -- 1 byte before method, next loop increments to function start byte
                                                   , fp = S.length remainingStack -- frame pointer points to start of new stack frame
                                                   , cpuStack = S.fromList [ip cpu + 1, fp cpu] <> remainingStack -- add return ip and current fp to stack
                                                   }) ""
```

* Call creates a new CPU, setting the ip, fp and stack frame as discussed above in the section about Call & Ret

```haskell
        Ret ->
          -- Return to the calling code, get the previous frame pointer (fp) and return address from the current stack frame.
          -- A ret always 'returns' a single value, the last item on the stack 
          let stackAtFp = S.drop (S.length remainingStack - fp cpu - 2) remainingStack in
          let (retParams, retStack) = S.splitAt 2 stackAtFp in
          if S.length retParams == 2 then
            let [retIp, retFp] = toList retParams in
            cpu { cpuStack = S.fromList popped <> retStack
                , ip = retIp - 1
                , fp = retFp
                }
          else
            cpu { panic = True, state = "Stack underflow getting return frame" }            
```

* Ret gets the old fp and the return address from the stack frame and constructs the new CPU to return to the caller. 

## What's next?

This explanation of the stack machine is significantly longer than the code for it :). Hopefully you'll be able to see how easy creating a simple stack machine is. There are many things than can be built on top of this. E.g.
 * A higher level assembler with support for labels, i.e. don’t make the user count offsets
 * A simple higher level language that generates this new assembler code. E.g. stack/concatenative language or a functional language
 * Many more operators  
 * Code optimisation
 * Support for globals
 * IO
 * .....



## Links
1. https://en.wikipedia.org/wiki/Stack_machine
2. https://github.com/sdiehl/protolude

## See also
- https://www.youtube.com/watch?v=OjaAToVkoTw

