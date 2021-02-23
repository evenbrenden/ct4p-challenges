#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

type Poly = [Int]
type Coeff = Int
type Power = Int

calc :: (Int, Int) -> Int -> Double
calc (coefficient, power) x = (fromIntegral coefficient) * ((fromIntegral x) ** (fromIntegral power))

indexed :: Poly -> [(Coeff, Power)]
indexed xs = zip xs [0..]

calcs :: Poly -> Int -> [Double]
calcs poly x = map (flip calc (fromIntegral x)) (indexed poly)

evalPoly :: Poly -> Int -> Double
evalPoly poly x = sum $ calcs poly x

main :: IO ()
main = do
  putStrLn $ show $ evalPoly [-1, 0, 4] 2 -- 4x^2 - 1, x = 2
  return ()
