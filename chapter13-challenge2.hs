#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

newtype Multi a = Multi a
  deriving Show

instance Num a => Semigroup (Multi a) where
  (Multi x) <> (Multi y) = Multi (x * y)

instance Num a => Monoid (Multi a) where
  mempty = Multi 1

h :: [Multi Int] -> Multi Int
h list = foldr (\num acc -> num <> acc) mempty list

-- Equational reasoning (won't compile), dropping Multi from here on.

-- h [2, 3] = 2 * 3 = h [2] * h [3]

-- What is the image of the empty list []?

-- h [] = 1

-- What's the image of [1, 2, 3, 4]?

-- h [1, 2, 3 ,4] = 4 * 3 * 2 * 1 = 24

-- How many different lists map to the integer 12?

-- 12 = 2 * 2 * 3 = 4 * 3 = 2 * 6

-- All permutations of a list of these numbers: [2, 3, 2], [3, 4], [2, 6], etc.
-- Since you can add any number of 1s, there is an infinite number of such lists.

-- Is there any other homomorphism between the two monoids?

h' :: [Int] -> Multi Int
h' _ = mempty

-- h' [2, 3] = 1 = h' [2] * h' [3]
