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

-- Square order not enforced sry
type Matrix = [[Int]]
type Row = [Int]
type Col = [Int]

add :: (Row, Row) -> Row
add (r1, r2) = zipWith (+) r1 r2

transpose :: Matrix -> Matrix
transpose ([]:_) = []
transpose x = map head x:transpose (map tail x)

multiply :: Row -> Col -> Int
multiply row col = foldr (+) 0 $ zipWith (*) row col

multiplyRow :: Matrix -> Row -> Row
multiplyRow sm row = multiply row <$> sm

evalSM :: Algebra RingF Matrix
evalSM RZero = [[]]
evalSM ROne = [[1]]
evalSM (RAdd sm1 sm2) = add <$> zip sm1 sm2
evalSM (RMul sm1 sm2) = multiplyRow sm1 <$> (transpose sm2)
evalSM (RNeg n) = (map . map $ (0-)) n

main :: IO ()
main = do
  let example = RMul [[2, 0], [3, 0]] [[2, 3], [0, 0]]
  putStrLn $ show $ evalSM example
  return ()
