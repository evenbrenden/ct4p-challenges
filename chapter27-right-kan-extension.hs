{-# LANGUAGE RankNTypes #-}

newtype Ran k d a = Ran (forall i. (a -> k i) -> d i)

-- Replacing d with the forgetful functor = identity functor, we can find the free functor from the category of monoids.
type Lst a = forall i. Monoid i => (a -> i) -> i

toLst :: [a] -> Lst a
toLst as = \f -> foldMap f as

-- Generate a list using the free functor from the category of monoids.
fromLst :: Lst a -> [a]
fromLst f = f (\a -> [a])

newtype Sum a = Sum a deriving Show

instance Num a => Semigroup (Sum a) where
    (Sum a) <> (Sum b) = Sum (a + b)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0

main :: IO ()
main = do
    -- k is Sum (the forgetful functor under the sum monoid)
    -- d is the identity functor (could have used Identity)
    -- a is an integer
    -- i is an integer
    let list = fromLst (toLst [1..5])
        sum = toLst [1..5] $ Sum
    putStrLn $ show list
    putStrLn $ show sum
    return ()
