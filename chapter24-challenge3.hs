#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p ghc
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

type Algebra f a = f a -> a

data Expr =
      RZero
    | ROne
    | RAdd Expr Expr
    | RMul Expr Expr
    | RNeg Expr

-- 2x2 matrix as (row, row)
type Matrix2x2 = ((Int, Int), (Int, Int))

add :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
add ((aa1, ab1), (ba1, bb1)) ((aa2, ab2), (ba2, bb2)) =
    ((aa1 + aa2, ab1 + ab2), (ba1 + ba2, bb1 + bb2))

mul :: Matrix2x2 -> Matrix2x2 -> Matrix2x2
mul ((aa1, ab1), (ba1, bb1)) ((aa2, ab2), (ba2, bb2)) =
    ((aa1 * aa2 + ab1 * ba2, aa1 * ab2 + ab1 * bb2), (ba1 * aa2 + bb1 * ba2, ba1 * ab2 + bb1 * bb2))

neg :: Matrix2x2 -> Matrix2x2
neg ((aa, ab), (ba, bb)) = ((-aa, -ab), (-ba, -bb))

evalZ :: Expr -> Matrix2x2
evalZ RZero = ((0, 0), (0, 0))
evalZ ROne = ((1, 0), (0, 1))
evalZ (RAdd m1 m2) = add (evalZ m1) (evalZ m2)
evalZ (RMul m1 m2) = mul (evalZ m1) (evalZ m2)
evalZ (RNeg m) = neg (evalZ m)

main :: IO ()
main = do
  let expr = RMul (RAdd ROne ROne) (RNeg ROne)
  putStrLn $ show $ evalZ expr
  return ()
