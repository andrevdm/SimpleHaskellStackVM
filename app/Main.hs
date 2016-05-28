{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protolude
import qualified Data.Text as Txt
import qualified Data.Sequence as S
import VirtualMachine

main :: IO ()
main = do
  {-
  let assembly = [ (Nop, [])
                 , (Push, [7])
                 , (Push, [15])
                 , (Push, [41])
                 , (Nop, [])
                 , (Pop, [])
                 , (Add, [])
                 , (Pop, [])
                 , (Jmp, [1])
                 , (Halt, [])
                 , (Nop, [])
                 , (Push, [1])
                 , (Dup, [])
                 , (Push, [5])
                 , (Bgt, [3] )
                 , (Inc, [])
                 , (Jmp, [-8])
                 , (Nop, [])
                 , (Nop, [])
                 ]
  let assembly = [(Push, [11]), (Call, [6]), (Nop, []), (Halt, []), (Break, []), (Push, [99]), (Ret, []), (Push, [1])]
  -}

  let assembly = [ (Push, [22])
                 , (Push, [123])
                 , (Call, [9])
                 , (PopPrev, [2])
                 , (Halt, [])
                 , (LdArg, [2])
                 , (LdArg, [1])
                 , (Add, [])
                 , (Ret, [])
                 ]

  let assembled = assembleByteCode assembly
  let (lines, cpus) = case assembled of
                         Left err ->
                           (Txt.intercalate "\n" $ map show err, [])
                         Right bytes -> 
                           let c = reverse $ interpretByteCode $ S.fromList bytes in
                           (showT bytes, c)

  putText lines
  putText "------------------------"
  putText $ Txt.intercalate "\n" $ map show cpus


  where
    showT :: (Show o) => o -> Text
    showT = show
