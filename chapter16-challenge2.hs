#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE RankNTypes #-}

btoa :: b -> a
btoa = fromY id

fromY :: (a -> x) -> b -> x
fromY f b = f . btoa $ b

btoa2FromY :: (b -> a) -> ((a -> x) -> (b -> x))
btoa2FromY btoa' = \f -> f . btoa'

fromYToBtoa :: forall a b. (forall x. (a -> x) -> (b -> x)) -> (b -> a)
fromYToBtoa fromY' = fromY' id

-- We can go from btoa to fromY and back => these mappings are the inverse of each other
