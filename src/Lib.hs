{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Protolude
import qualified Data.Sequence as S

-- | from https://stackoverflow.com/questions/2743858/safe-and-polymorphic-toenum/2744712#2744712
enumIfBetween :: (Enum a) => a -> a -> Int -> Maybe a
enumIfBetween a z x = let a' = fromEnum a
                          z' = fromEnum z
                      in if a' <= x && x <= z'
                         then Just $ toEnum x
                         else Nothing

-- | A safe from enum to avoid, returns a Maybe rather than having to worry about ⊥
toEnumMay :: (Bounded a, Enum a) => Int -> Maybe a
toEnumMay i = enumIfBetween minBound maxBound i

-- | Safe version of Data.Seq.index, avoiding ⊥
indexMay :: S.Seq a -> Int -> Maybe a
indexMay s idx 
  | idx < 0 = Nothing
  | idx >= S.length s = Nothing
  | otherwise = Just $ S.index s idx
  
  
