#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE GADTs #-}

import Data.Functor.Identity

data Lan k d a = forall i . Lan (k i -> a) (d i)

type Exp a b = Lan ((,) a) Identity b

toExp :: (a -> b) -> Exp a b
toExp f = Lan (f . fst) (Identity ())

fromExp :: Exp a b -> (a -> b)
fromExp (Lan f (Identity x)) = \a -> f (a, x)

main :: IO ()
main = do
    let exp' = fromExp (toExp (==(1::Int)))
    putStrLn $ show $ exp' 1
    putStrLn $ show $ exp' 2
    return ()
