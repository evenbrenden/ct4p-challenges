#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

{-# LANGUAGE TypeFamilies #-}

class Representable f where
  type Rep f :: *
  tabulate :: (Rep f -> x) -> f x
  index :: f x -> Rep f -> x

data Stream x = Cons x (Stream x) deriving Show

instance Representable Stream where
  type Rep Stream = Int
  tabulate f = Cons (f 0) (tabulate (f . (+1)))
  index (Cons b bs) n = if n == 0 then b else index bs (n - 1)

square :: Int -> Int
square x = x * x

memo :: Stream Int
memo = tabulate square

memoSquares :: [Int]
memoSquares = fmap (index memo) [1..4]

main :: IO ()
main = do
  putStrLn $ show memoSquares
  return ()
