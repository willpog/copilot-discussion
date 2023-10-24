{-# LANGUAGE RebindableSyntax #-}

module Copilot.Library.Voting
  ( majority, aMajority ) where

import Copilot.Language
import qualified Prelude as P

-- | Majority vote first pass: choosing a candidate.
majority :: (P.Eq a, Typed a) =>
            [Stream a] -- ^ Vote streams
            -> Stream a -- ^ Candidate stream
majority []     = badUsage "majority: empty list not allowed"
majority (x:xs) = majority' xs x 1

-- Alternate syntax of local bindings.
majority' :: (P.Eq a, Typed a)
   => [Stream a] -> Stream a -> Stream Word32 -> Stream a
majority' []     can _   = can
majority' (x:xs) can cnt =
  local (cnt == 0) inZero
  where
  inZero zero    = local (if zero then x else can) inCan
    where
    inCan can'   = local (if zero || x == can then cnt+1 else cnt-1) inCnt
      where
      inCnt cnt' = majority' xs can' cnt'

-- | Majority vote second pass: checking that a candidate indeed has more than
-- half the votes.
aMajority :: (P.Eq a, Typed a) =>
             [Stream a] -- ^ Vote streams
             -> Stream a -- ^ Candidate stream
             -> Stream Bool -- ^ True if candidate holds majority
aMajority [] _ = badUsage "aMajority: empty list not allowed"
aMajority xs can =
  let
    cnt = aMajority' 0 xs can
  in
    (cnt * 2) > fromIntegral (length xs)

aMajority' :: (P.Eq a, Typed a)
  => Stream Word32 -> [Stream a] -> Stream a -> Stream Word32
aMajority' cnt []     _   = cnt
aMajority' cnt (x:xs) can =
  local (if x == can then cnt+1 else cnt) $ \ cnt' ->
    aMajority' cnt' xs can
