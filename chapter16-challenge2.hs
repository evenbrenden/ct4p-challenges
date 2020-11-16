#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE RankNTypes #-}

btoa:: b -> a
btoa = fromY id

fromY:: (a -> x) -> b -> x
fromY f b = f (btoa b)

forward':: (b -> a) -> ((a -> x) -> (b -> x))
forward' btoa' = (\f -> f . btoa')

backward' :: (forall x. (a -> x) -> (b -> x)) -> (b -> a)
backward' aToxToBtox = aToxToBtox id
