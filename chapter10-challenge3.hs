#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall'"

data Reader a b = Reader (a -> b)

natTrans1 :: Reader Bool a -> Maybe a
natTrans1 (Reader f) = Just (f True)

natTrans2 :: Reader Bool a -> Maybe a
natTrans2 (Reader f) = Just (f False)

natTrans3 :: Reader Bool a -> Maybe a
natTrans3 _ = Nothing

-- That's all of them.
