#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

import Data.Functor.Contravariant

-- Setup

newtype Opp r a = Opp (a -> r) -- Because Op is taken

instance Contravariant (Opp r) where
  -- contramap :: (b -> a) -> Opp (a -> r) -> Opp (b -> r)
  contramap f (Opp g) = Opp (g . f)

unwrapOpp :: (Opp r a) -> a -> r
unwrapOpp (Opp f) x = f x

------------------
-- Test case #1
------------------

-- Helpers
predToStr :: Opp Bool a -> Opp String a
predToStr (Opp f) = Opp (\x -> if f x then "T" else "F")

f1 :: Int -> Bool
f1 x = x > 0

testOpp1 :: Opp Bool Bool
testOpp1 = Opp id

testArg1 :: Int
testArg1 = 1

-- Naturality condition
leftSide1 :: Opp Bool Bool -> Opp String Int
leftSide1 = contramap f1 . predToStr

rightSide1 :: Opp Bool Bool -> Opp String Int
rightSide1 = predToStr . contramap f1

leftSideApplied1 :: String
leftSideApplied1 = unwrapOpp (leftSide1 testOpp1) testArg1

rightSideApplied1 :: String
rightSideApplied1 = unwrapOpp (rightSide1 testOpp1) testArg1

-- Test
result1 :: Bool
result1 = leftSideApplied1 == rightSideApplied1 -- => True

------------------
-- Test case #2
------------------

-- Helpers
predToBool :: Opp Int a -> Opp Bool a
predToBool (Opp f) = Opp (\x -> f x == 0)

f2 :: String -> Int
f2 _ = 1

testOpp2 :: Opp Int Int
testOpp2 = Opp (\x -> x - 1)

testArg2 :: String
testArg2 = "T"

-- Naturality condition
leftSide2 :: Opp Int Int -> Opp Bool String
leftSide2 = contramap f2 . predToBool

rightSide2 :: Opp Int Int -> Opp Bool String
rightSide2 = predToBool . contramap f2

leftSideApplied2 :: Bool
leftSideApplied2 = unwrapOpp (leftSide2 testOpp2) testArg2

rightSideApplied2 :: Bool
rightSideApplied2 = unwrapOpp (rightSide2 testOpp2) testArg2

-- Test
result2 :: Bool
result2 = leftSideApplied2 == rightSideApplied2 -- => True

main :: IO ()
main = do
  print result1
  print result2
  return ()
