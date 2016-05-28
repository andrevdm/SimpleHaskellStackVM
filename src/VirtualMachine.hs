{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module VirtualMachine (Operation(..), Cpu(..), assembleByteCode, interpretByteCode) where

import Protolude
import qualified Data.Sequence as S
--import Data.Sequence((<|), (|>), (><), ViewL(..), ViewR(..))
import qualified Data.Map as Map
import Lib

-- | The op codes
data Operation = Nop       -- No operation
               | Break     -- Break for debugger - not implemented
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

{-| Representation of the current CPU state
ip = instruction pointer
fp = frame pointer, points to the start of the stack frame
cpuStack = the stack 
cpuGlobals = global variables
runOp = op that was executed
state = debug text
debug = enable debugging features
panic = interpreter will stop when this is set to true


This is how the current stack frame is setup

               |    +-----------------+
               |    | existing stack  |
               |    +-----------------+ 
               |    | arg 2           |
               |    | arg 1           |
               |    +-----------------+ <-------- fp
               |    | old fp          |
               |    | return address  |
               |    +-----------------+
               |    | method's stack  |
               |    +-----------------+
               V
             stack 
             growth

The fp points to the start of a stack frame. When a return instruction is executed the following takes place
  1) Pop the return value (there is always one return value)
  2) Rewind the stack to the current value of the fp + 2
  3) pop the return address and set the ip
  4) pop the old fp value and set the current fp
  5) caller pops the returrn value
  6) caller pops paremeters off stack (cdecl convention)

Note that return value is after the args passed to the function, i.e. steps 5 and 6 above are logically backwards.
The PopPrev op can be used to make this simpler as then steps 5 and 6 can be swapped, i.e. first PopPrev {n}, then pop {return value}
-}
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
emptyCpu = Cpu { ip = -1, fp = -1, cpuStack = S.empty, cpuGlobals = S.empty, state = "", debug = True, ranOp = 0, panic = False }

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

-- | Config for the op codes
instructions :: [Instruction]
instructions = [ Instruction { opCode = Nop, opParamCount = 0, opPopsCount = 0, opSimple = True }
               , Instruction { opCode = Break, opParamCount = 0, opPopsCount = 0, opSimple = True }
               , Instruction { opCode = Halt, opParamCount = 0, opPopsCount = 0, opSimple = True }
               , Instruction { opCode = Push, opParamCount = 1, opPopsCount = 0, opSimple = True }
               , Instruction { opCode = Pop, opParamCount = 0, opPopsCount = 1, opSimple = True }
               , Instruction { opCode = PopPrev, opParamCount = 1, opPopsCount = 0, opSimple = False }
               , Instruction { opCode = Add, opParamCount = 0, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Inc, opParamCount = 0, opPopsCount = 1, opSimple = True }
               , Instruction { opCode = Dup, opParamCount = 0, opPopsCount = 1, opSimple = True }
               , Instruction { opCode = Jmp, opParamCount = 1, opPopsCount = 0, opSimple = True }
               , Instruction { opCode = Bne, opParamCount = 1, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Beq, opParamCount = 1, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Bgt, opParamCount = 1, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Bgte, opParamCount = 1, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Blt, opParamCount = 1, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Blte, opParamCount = 1, opPopsCount = 2, opSimple = True }
               , Instruction { opCode = Call, opParamCount = 1, opPopsCount = 0, opSimple = False }
               , Instruction { opCode = Ret, opParamCount = 0, opPopsCount = 1, opSimple = False }
               , Instruction { opCode = LdArg, opParamCount = 1, opPopsCount = 0, opSimple = False }
               ]

-- | Instructions indexed by opcode
instrByOp :: Map.Map Operation Instruction
instrByOp = Map.fromList $ map (\i -> (opCode i, i)) instructions

-- | A single assembler error
data AssemblerError = AssemblerError Integer Operation Text deriving (Show, Eq)

-- | A single CPU operator and its parameters
type OpAndParam = (Operation, [Int])


-- | Compiles the list to byte code
-- | Returns as many errors as possible rather than just first error
assembleByteCode :: [OpAndParam] -> Either [AssemblerError] [Int]
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


-- | Interpreter for the byte code
-- | Given a byte code stream will run the code
-- | If debug is enabled then the full history (all states) will be returned. TODO currently always enabled
interpretByteCode :: S.Seq Int -> [Cpu]
interpretByteCode byteCode =
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

    -- | Simple instructions, can not directly change CPU state, e.g. cant set ip/fp and they just return data to be added to stack
    interpretSimple :: Cpu -> Operation -> [Int] -> S.Seq Int -> Cpu
    interpretSimple cpu op popped params =
      case op of
        Nop -> cpu 
        Break -> cpu --TODO not implemented
        Halt -> cpu { panic = True }
        Push -> cpu { cpuStack = params }
        Pop -> cpu
        Bne -> branchIf cpu popped params (/=)
        Beq -> branchIf cpu popped params (==)
        Bgt -> branchIf cpu popped params (>)
        Bgte -> branchIf cpu popped params (>=)
        Blt -> branchIf cpu popped params (<)
        Blte -> branchIf cpu popped params (<=)
        Inc -> singleVal cpu popped (\v -> cpu { cpuStack = S.singleton $ v + 1 }) "IncI stack underflow"
        Dup -> singleVal cpu popped (\v -> cpu { cpuStack = S.fromList [v, v] }) "Dup stack underflow"
        Jmp -> singleVal cpu (toList params) (\v -> cpu { ip = ip cpu + v }) "Jmp missing param"
        Add -> cpu { cpuStack = S.singleton $ sum popped }
        _ -> cpu { panic = True, state = "Unhandled simple op" }

    -- | Complex instructions have full access to the CPU and must ensure the CPU is correctly setup for the next instruction
    interpretComplex :: Cpu -> Operation -> [Int] -> S.Seq Int -> S.Seq Int -> Cpu
    interpretComplex cpu op popped params remainingStack =
      case op of
        Call ->
          -- Set up a stack frame and jump to the call location. See the comments above for details of the stack frame
          singleVal cpu (toList params) (\v -> cpu { ip = v - 1 -- 1 byte before method, next loop increments to function start byte
                                                   , fp = S.length remainingStack -- frame pointer points to start of new stack frame
                                                   , cpuStack = S.fromList [ip cpu + 1, fp cpu] <> remainingStack -- add return ip and current fp to stack
                                                   }) ""
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
            
        PopPrev ->
          -- Keep the top item on the stack but remove the previous n items
          case toList $ S.take 1 params of
            [nr] ->
              let (h,t) = S.splitAt (nr + 1) remainingStack in
              if S.length h == nr + 1
              then
                cpu { cpuStack = S.take 1 h <> t }
              else
                cpu { panic = True, state = "Stack underflow in pop prev" }
            [] -> cpu { panic = True, state = "Code underflow getting number of items to pop prev" }
            _ -> cpu { panic = True, state = "Invalid number of items to pop prev" }
  
        LdArg ->
          singleVal cpu (toList params) (\v ->
                                           case indexMay remainingStack (length remainingStack - fp cpu + v - 1) of
                                             Nothing -> cpu { panic = True, state = "Stack underflow reading local arg in Ldarg"}
                                             Just l -> cpu { cpuStack = S.singleton l <> remainingStack }
                                        ) "Ldarg code underflow"
  
        _ -> cpu { panic = True, state = "Unhandled complex op" }

    -- | Get a single value from the list, if there is not a value available, return an error
    singleVal :: Cpu -> [Int] -> (Int -> Cpu) -> Text -> Cpu
    singleVal cpu params onOne err =
      case head params of
        Nothing -> cpu { panic = True, state = err }
        Just v -> onOne v

    -- | Perform a branch instruction if the predicate for the op (beq, bne, bgte etc) is true
    branchIf :: Cpu -> [Int] -> S.Seq Int -> (Int -> Int -> Bool) -> Cpu
    branchIf cpu popped params prd =
      case head params of
        Nothing -> cpu { panic = True, state = "missing param" }
        Just jmp ->
          case popped of
            [b, a] -> cpu { ip = if prd a b then ip cpu + jmp else ip cpu } 
            _ -> cpu { panic = True, state = "invalid stack" }
