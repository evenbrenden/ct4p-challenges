#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

data Option a = Some a | Nada deriving Show

(>=>) :: (a -> Option b) -> (b -> Option c) -> (a -> Option c)
(>=>) amb bmc = \a ->
    case amb a of
        Some b ->
            bmc b
        Nada -> Nada

return' :: a -> Option a
return' = Some

safe_reciprocal :: (Eq a, Fractional a) => a -> Option a
safe_reciprocal x = if x == 0 then Nada else Some (1/x)

safe_root :: (Ord a, Floating a) => a -> Option a
safe_root x = if x >= 0 then Some (sqrt x) else Nada

safe_root_reciprocal :: Double -> Option Double
safe_root_reciprocal = safe_root >=> safe_reciprocal

main :: IO ()
main = do
    putStrLn $ show $ safe_root_reciprocal (0.25 :: Double)
