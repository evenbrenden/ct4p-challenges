#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

type Algebra f a = f a -> a

data RingF a = RZero
             | ROne
             | RAdd a a
             | RMul a a
             | RNeg a

type Poly = [Int]

sum' :: (Int, Int) -> Int
sum' (x, y) = x + y

zipPad :: [Int] -> [Int] -> [(Int, Int)]
zipPad (x:xs) (y:ys) = (x,y):zipPad xs ys
zipPad [] ys = zip (repeat 0) ys
zipPad xs [] = zip xs (repeat 0)

zipWithSumPad :: [Int] -> [Int] -> [Int]
zipWithSumPad xs ys = sum' <$> zipPad xs ys

makeShifts :: [Int] -> Int -> [[Int]]
makeShifts xs n =
    if n == 0 then [xs]
    else makeShifts xs (n - 1) ++ [(replicate n 0 ++ xs)]

multiZip :: [Int] -> [[Int]] -> [[Int]]
multiZip xs ys = zipWith (\x ys' -> (*x) <$> ys') xs ys

concatZipSum :: [[Int]] -> [Int]
concatZipSum (m:ms) = zipWithSumPad m (concatZipSum ms)
concatZipSum [] = []

evalP :: Algebra RingF Poly
evalP RZero = []
evalP ROne = [1]
evalP (RAdd xs ys) = zipWithSumPad xs ys
evalP (RMul xs ys) = concatZipSum $ multiZip xs (makeShifts ys (length xs))
evalP (RNeg n) = map (0-) n

main :: IO ()
main = do
  let binomial = RMul [1, 2] [2, 1]
  putStrLn $ show $ evalP binomial
  return ()
