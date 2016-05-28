{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Protolude
import VirtualMachine
import Test.Hspec
import Test.QuickCheck
import qualified Data.Sequence as S

runAndGetStack :: [(Operation, [Int])] -> [Int]
runAndGetStack assembly =
  case assembleByteCode assembly of
    Left _ ->
      [-1]
    Right bytes ->
      case interpretByteCode $ S.fromList bytes of
        [] -> [-1]
        (cpu:_) -> toList $ cpuStack cpu
  
main :: IO ()
main = hspec $ do
  describe "check stack result" $ do
    it "nop does nothing" $ do
      let res = runAndGetStack [ (Nop, []) ]
      res `shouldBe` []

    it "nops do nothing" $ do
      let res = runAndGetStack [ (Nop, []), (Nop, []), (Nop, []) ]
      res `shouldBe` []

    it "pushi" $ do
      let res = runAndGetStack [ (Push, [123]) ]
      res `shouldBe` [123]

    it "popi" $ do
      let res = runAndGetStack [ (Push, [123]), (Pop, []) ]
      res `shouldBe` []

    it "addi" $ do
      let res = runAndGetStack [ (Push, [123]), (Push, [415]), (Add, []) ]
      res `shouldBe` [538]

  describe "stack result property tests" $ do
    it "pushi" $ property $
      \x -> runAndGetStack [ (Push, [x])] == [(x :: Int)]

    it "pushi multiple" $ property $
      \x -> (runAndGetStack $ map (\y -> (Push, [y])) x) == reverse (x :: [Int])

    it "addi" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Add, []) ] == [(x :: Int) + (y :: Int)]

    it "inci" $ property $
      \x -> runAndGetStack [ (Push, [x]), (Inc, []) ] == [(x :: Int) + 1]

    it "dup" $ property $
      \x -> runAndGetStack [ (Push, [x]), (Dup, []) ] == [(x :: Int), x]

    it "eq" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Beq, [1]), (Push, [99]) ] == if (x :: Int) == (y :: Int) then [] else [99]

    it "ne" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Bne, [1]), (Push, [99]) ] == if (x :: Int) /= (y :: Int) then [] else [99]

    it "gt" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Bgt, [1]), (Push, [99]) ] == if (x :: Int) > (y :: Int) then [] else [99]

    it "gte" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Bgte, [1]), (Push, [99]) ] == if (x :: Int) >= (y :: Int) then [] else [99]

    it "lt" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Blt, [1]), (Push, [99]) ] == if (x :: Int) < (y :: Int) then [] else [99]

    it "lte" $ property $
      \x y -> runAndGetStack [ (Push, [x]), (Push, [y]), (Blte, [1]), (Push, [99]) ] == if (x :: Int) <= (y :: Int) then [] else [99]

    it "count with jmp and bgt" $ property $
      \x -> runAndGetStack
            [ (Push, [0])
            , (Dup, [])
            , (Push, [abs x])
            , (Bgt, [3] )
            , (Inc, [])
            , (Jmp, [-8])
            , (Nop, [])
            ] == [(abs (x :: Int)) + 1]

    it "add with call, ldard and ret" $ property $
      \x y -> runAndGetStack
              [ (Push, [abs x])
              , (Push, [abs y])
              , (Call, [10])
              , (Nop, [])
              , (PopPrev, [2])
              , (Halt, [])
              , (LdArg, [1])
              , (LdArg, [2])
              , (Add, [])
              , (Ret, [])
              ] == [(abs (x :: Int)) + (abs (y :: Int))]
