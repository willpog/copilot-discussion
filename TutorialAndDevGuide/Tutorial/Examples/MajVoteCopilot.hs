{-# LANGUAGE RebindableSyntax #-}

module Main where

import Language.Copilot

nums :: Stream Int32
nums = [1] ++ nums + 1

vote :: Spec
vote = do
  -- majority selects an element that occurs the most times in input.
  trigger "maj"  true [arg maj]

  -- aMajority checks if the selected element has a majority.
  trigger "aMaj" true [arg $ aMajority inputs maj]

  observer "nums" ([0] ++ nums)
  observer "nums/1" a
  observer "nums/2" b
  observer "nums/3" c
  observer "nums/4" d

  where
    maj = majority inputs

    -- 4 input streams to vote on
    inputs :: [Stream Bool]
    inputs = [ a, b, c, d]

    a = [True] ++ (nums `mod` 1 == 0)
    b = [True] ++ (nums `mod` 2 == 0)
    c = [True] ++ (nums `mod` 3 == 0)
    d = [True] ++ (nums `mod` 4 == 0)

main :: IO ()
main = interpret 30 vote
