-- 1

natTrans :: Maybe a -> [a]
natTrans (Just a) = [a]
natTrans Nothing = []

fmap f (natTrans Nothing) = fmap f [] = [] = natTrans Nothing = natTrans (fmap f Nothing)
fmap f (natTrans (Just a)) = fmap f [a] = [f a] = natTrans (Just (f a)) = natTrans (fmap f (Just a))

-- 2

newtype Reader a b = Reader (a -> b)

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

natTrans1 :: Reader () a -> [a]
natTrans1 (Reader _) = []

-- Proof
\rg -> fmap f (natTrans1 rg) = \rg -> fmap f [] = \_ -> []
\rg -> natTrans1 (fmap f rg) = \(Reader g) -> natTrans1 (Reader $ f . g) = \_ -> []

natTrans2 :: Reader () a -> [a]
natTrans2 (Reader f) = [f ()]

-- Proof
\rg -> fmap f (natTrans2 rg) = \(Reader g) -> fmap f [g ()] = \(Reader g) -> [f (g ())] = \(Reader g) -> [f . g ()]
\rg -> natTrans2 (fmap f rg) = \(Reader g) -> natTrans2 (Reader $ f . g) = \(Reader g) -> [f . g ()]

natTrans3 :: Reader () a -> [a]
natTrans3 (Reader g) = fmap g [(), ()]

-- Proof
\rg -> fmap f (natTrans3 rg) = \(Reader g) -> fmap f (fmap g [(), ()]) = \(Reader g) -> fmap f [g (), g ()] = \(Reader g) -> [f . g (), f . g ()]
\rg -> natTrans3 (fmap f rg)  = \(Reader g) -> natTrans3 (Reader $ f . g) = \(Reader g) -> fmap (f . g) [(), ()] = \(Reader g) -> [f . g (), f . g ()]

-- There are infinitely many [(),...,()] lists => infinitely many natural transformations from Reader () to [].

-- 3

natTrans1 :: Reader Bool a -> Maybe a
natTrans1 (Reader f) = Just (f True)

natTrans2 :: Reader Bool a -> Maybe a
natTrans2 (Reader f) = Just (f False)

natTrans3 :: Reader Bool a -> Maybe a
natTrans3 _ = Nothing

-- That's all of them.

-- 6

import Data.Functor.Contravariant

newtype Opp r a = Opp (a -> r)

instance Contravariant (Opp r) where
  contramap f (Opp g) = Opp (g . f)

unwrapOp (Opp f) x = f x

-- Testing this:

predToStr :: Opp Bool b -> Opp String b
predToStr (Opp f) = Opp (\x -> if f x then "T" else "F")

f1 :: Int -> Bool
f1 x = x > 0

unwrapOp (contramap f . predToStr $ Op (\x -> True)) 1 -- "T"
unwrapOp (predToStr . contramap f $ Op (\x -> True)) 1 -- "T"

-- Testing that:

anotherNatTrans :: Opp Integer a -> Opp Bool a
anotherNatTrans (Opp f) = Opp (\x -> f x == 0)

f2 :: String -> Integer
f2 x = 1

unwrapOp (contramap f2 . anotherNatTrans $ Opp (\x -> x - 1)) $ "T" -- True
unwrapOp (anotherNatTrans . contramap f2 $ Opp (\x -> x - 1)) $ "T" -- True

