#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

product2Function :: ((z, a) -> b) -> (z -> (a -> b))
product2Function za_b = \z -> (\a -> za_b (z, a))

function2Product :: (z -> (a -> b)) -> ((z, a) -> b)
function2Product z_ab = \za -> (z_ab (fst za) (snd za))
