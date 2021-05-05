#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

-- (Also inspired by http://danshiebler.com/2018-11-10-category-solutions/)

leftAdjunct :: ((z, a) -> b) -> (z -> (a -> b))
leftAdjunct za_b = \z -> (\a -> za_b (z, a))

rightAdjunct :: (z -> (a -> b)) -> ((z, a) -> b)
rightAdjunct z_ab = \za -> (z_ab (fst za) (snd za))
