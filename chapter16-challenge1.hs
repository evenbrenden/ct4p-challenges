#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

-- Co-Yoneda embedding can be represented as:
--
-- (x -> a) -> (x -> b) ≅ a -> b

fromY :: (x -> a) -> x -> b
fromY f x = atob (f x)

atob :: a -> b
atob = fromY id
