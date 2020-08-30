-- Challenge 1

natTrans :: Maybe a -> [a]
natTrans (Just a) = [a]
natTrans Nothing = []

--Proof
fmap f (natTrans Nothing) =
fmap f [] =
[] =
natTrans Nothing =
natTrans (fmap f Nothing)

fmap f (natTrans (Just a)) =
fmap f [a] =
[f a] =
natTrans (Just (f a)) =
natTrans (fmap f (Just a))

-- Challenge 2

newtype Reader a b = Reader (a -> b)

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

natTrans1 :: Reader () a -> [a]
natTrans1 (Reader _) = []

-- Proof
\rg -> fmap f (natTrans1 rg) =
\rg -> fmap f [] =
\_ -> [] =
\(Reader g) -> natTrans1 (Reader $ f . g) =
\rg -> natTrans1 (fmap f rg)

natTrans2 :: Reader () a -> [a]
natTrans2 (Reader f) = [f ()]

-- Proof
\rg -> fmap f (natTrans2 rg) =
\(Reader g) -> fmap f [g ()] =
\(Reader g) -> [f (g ())] =
\(Reader g) -> [f . g ()] =
\(Reader g) -> natTrans2 (Reader $ f . g) =
\rg -> natTrans2 (fmap f rg)

natTrans3 :: Reader () a -> [a]
natTrans3 (Reader g) = fmap g [(), ()]

-- Proof
\rg -> fmap f (natTrans3 rg) =
\(Reader g) -> fmap f (fmap g [(), ()]) =
\(Reader g) -> fmap f [g (), g ()] =
\(Reader g) -> [f . g (), f . g ()] =
\(Reader g) -> fmap (f . g) [(), ()] =
\(Reader g) -> natTrans3 (Reader $ f . g) =
\rg -> natTrans3 (fmap f rg)

-- There are infinitely many [(),...,()] lists => infinitely many natural transformations from Reader () to [].

-- Challenge 3

natTrans1 :: Reader Bool a -> Maybe a
natTrans1 (Reader f) = Just (f True)

natTrans2 :: Reader Bool a -> Maybe a
natTrans2 (Reader f) = Just (f False)

natTrans3 :: Reader Bool a -> Maybe a
natTrans3 _ = Nothing

-- That's all of them.

-- Challenge 6

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
leftSide1 = contramap f1 . predToStr
rightSide1 = predToStr . contramap f1

leftSideApplied1 = unwrapOpp (leftSide1 testOpp1) testArg1
rightSideApplied1 = unwrapOpp (rightSide1 testOpp1) testArg1

-- Test
result1 = leftSideApplied1 == rightSideApplied1 -- => True

------------------
-- Test case #2
------------------

-- Helpers
predToBool :: Opp Int a -> Opp Bool a
predToBool (Opp f) = Opp (\x -> f x == 0)

f2 :: String -> Int
f2 x = 1

testOpp2 :: Opp Int Int
testOpp2 = Opp (\x -> x - 1)

testArg2 :: String
testArg2 = "T"

-- Naturality condition
leftSide2 = contramap f2 . predToBool
rightSide2 = predToBool . contramap f2

leftSideApplied2 = unwrapOpp (leftSide2 testOpp2) testArg2
rightSideApplied2 = unwrapOpp (rightSide2 testOpp2) testArg2

-- Test
result2 = leftSideApplied2 == rightSideApplied2 -- => True

main = do
  print result1
  print result2
  return ()
