#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

-- Evaluate a polynomial at a point x

type Poly = [Int]

calc :: (Int, Int) -> Int -> Double
calc (coefficient, power) x = (fromIntegral coefficient) * ((fromIntegral x) ** (fromIntegral power))

indexed :: Poly -> [(Int, Int)]
indexed xs = zip xs [0..]

calcs :: Poly -> Int -> [Double]
calcs poly x = map (flip calc (fromIntegral x)) (indexed poly)

evalPoly :: Poly -> Int -> Double
evalPoly poly x = sum $ calcs poly x

-- Algebra for a ring of polynomials

type Algebra f a = f a -> a

data RingF a = RZero
             | ROne
             | RAdd a a
             | RMul a a
             | RNeg a

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

evalZ :: Algebra RingF Poly
evalZ RZero = []
evalZ ROne = [1]
evalZ (RAdd xs ys) = zipWithAddPad xs ys
evalZ (RMul xs ys) = concatZipAdd $ multiZip xs (makeShifts ys (length xs))
evalZ (RNeg n) = map (0-) n

main :: IO ()
main = do
  let poly1 = [-1, 0, 4] -- 4x^2 - 1
  let poly2 = [0, 1] -- 1x
  let mixed = evalZ (RMul poly1 (evalZ (RNeg poly2)))
  let x = 2
  putStrLn $ show $ evalPoly mixed x
  return ()
