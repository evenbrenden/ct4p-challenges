#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE DeriveFunctor #-}

-- From the book
newtype Fix f = Fix (f (Fix f))

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

data StreamF e a = StreamF e a deriving Functor

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

toListC :: Fix (StreamF e) -> [e]
toListC = cata al
  where al :: StreamF e [e] -> [e]
        al (StreamF e a) = e : a

-- A coalgebra that produces a list of squares of natural numbers
squaringCoalg :: [Int] -> StreamF Int [Int]
squaringCoalg (number:numbers) = StreamF (number^(2::Int)) numbers
squaringCoalg [] = StreamF 0 []

squares' :: Fix (StreamF Int)
squares' = ana squaringCoalg [0..]

squares :: [Int]
squares = toListC squares'

main :: IO ()
main = do
  -- let someSquares = take 10 $ squares
  putStrLn $ show someSquares
  return ()
