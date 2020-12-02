-- Definition by natural isomorphism (also inspired by
-- http://danshiebler.com/2018-11-10-category-solutions/). Alternatively (and
-- maybe how the challenge is meant to be solved), one could declare and
-- implement an Adjunction instance with the two types.

#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

product2Function :: ((z, a) -> b) -> (z -> (a -> b))
product2Function za_b = \z -> (\a -> za_b (z, a))

function2Product :: (z -> (a -> b)) -> ((z, a) -> b)
function2Product z_ab = \za -> (z_ab (fst za) (snd za))
