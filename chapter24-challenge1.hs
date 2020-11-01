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

add :: (Int, Int) -> Int
add (x, y) = x + y

zipPad :: Poly -> Poly -> [(Int, Int)]
zipPad (x:xs) (y:ys) = (x,y):zipPad xs ys
zipPad [] ys = zip (repeat 0) ys
zipPad xs [] = zip xs (repeat 0)

zipWithAddPad :: Poly -> Poly -> Poly
zipWithAddPad xs ys = add <$> zipPad xs ys

makeShifts :: Poly -> Int -> [Poly]
makeShifts xs n =
    if n == 0 then [xs]
    else makeShifts xs (n - 1) ++ [replicate n 0 ++ xs]

scale :: Int -> Poly -> Poly
scale x ys = (*x) <$> ys

multiZip :: Poly -> [Poly] -> [Poly]
multiZip xs yss = zipWith scale xs yss

concatZipAdd :: [Poly] -> Poly
concatZipAdd (m:ms) = zipWithAddPad m (concatZipAdd ms)
concatZipAdd [] = []

evalP :: Algebra RingF Poly
evalP RZero = []
evalP ROne = [1] -- Hm
evalP (RAdd xs ys) = zipWithAddPad xs ys
evalP (RMul xs ys) = concatZipAdd $ multiZip xs (makeShifts ys (length xs))
evalP (RNeg n) = map (0-) n

main :: IO ()
main = do
  let binomial = RMul [1, 2] [2, 1]
  putStrLn $ show $ evalP binomial
  return ()
