#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

newtype Reader a b = Reader (a -> b)

instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g

natTrans1 :: Reader () a -> [a]
natTrans1 (Reader _) = []

-- Proof:
--
-- \rg -> fmap f (natTrans1 rg) =
-- \rg -> fmap f [] =
-- \_ -> [] =
-- \(Reader g) -> natTrans1 (Reader $ f . g) =
-- \rg -> natTrans1 (fmap f rg)

natTrans2 :: Reader () a -> [a]
natTrans2 (Reader f) = [f ()]

-- Proof:
--
-- \rg -> fmap f (natTrans2 rg) =
-- \(Reader g) -> fmap f [g ()] =
-- \(Reader g) -> [f (g ())] =
-- \(Reader g) -> [f . g ()] =
-- \(Reader g) -> natTrans2 (Reader $ f . g) =
-- \rg -> natTrans2 (fmap f rg)

natTrans3 :: Reader () a -> [a]
natTrans3 (Reader g) = fmap g [(), ()]

-- Proof:
--
-- \rg -> fmap f (natTrans3 rg) =
-- \(Reader g) -> fmap f (fmap g [(), ()]) =
-- \(Reader g) -> fmap f [g (), g ()] =
-- \(Reader g) -> [f . g (), f . g ()] =
-- \(Reader g) -> fmap (f . g) [(), ()] =
-- \(Reader g) -> natTrans3 (Reader $ f . g) =
-- \rg -> natTrans3 (fmap f rg)

-- There are infinitely many [(),...,()] lists => infinitely many natural transformations from Reader () to [].
