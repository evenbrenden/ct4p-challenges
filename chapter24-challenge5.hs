#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr bmab b =
  let go :: (b -> Maybe (a, b)) -> Maybe (a, b) -> [a]
      go _ Nothing = []
      go f (Just (x, y)) = x : go f (f y)
  in go bmab (bmab b)

eratosthenes :: [Int] -> Maybe (Int, [Int])
eratosthenes (prime:numbers) =
  let isPrime prime' number = mod number prime' /= 0
  in Just (prime, filter (isPrime prime) numbers)
eratosthenes [] = Nothing

primes :: [Int]
primes = unfoldr eratosthenes [2..]

main :: IO ()
main = do
  let somePrimes = take 10 primes
  putStrLn $ show somePrimes
  return ()
