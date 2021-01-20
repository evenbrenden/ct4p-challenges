#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

iso1 :: Maybe a -> Either a ()
iso1 (Just a) = Left a
iso1 Nothing = Right ()

inv1 :: Either a () -> Maybe a
inv1 (Left a) = Just a
inv1 (Right _) = Nothing
