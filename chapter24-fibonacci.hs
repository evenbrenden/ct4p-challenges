#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DeriveFunctor #-}

newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

data NatF a = ZeroF | SuccF a deriving Functor

fib :: NatF (Int, Int) -> (Int, Int)
fib ZeroF = (0, 1)
fib (SuccF (m, n)) = (n, m + n)

fixNat :: Fix NatF
fixNat = Fix (SuccF (Fix (SuccF (Fix (SuccF (Fix ZeroF))))))

main :: IO ()
main = do
  let fourthAndFifthFibo = cata fib fixNat
  putStrLn $ show fourthAndFifthFibo
  return ()
