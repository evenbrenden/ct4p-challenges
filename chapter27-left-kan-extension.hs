{-# LANGUAGE GADTs #-}

import Data.Functor.Identity

data Lan k d a = forall i. Lan (k i -> a) (d i)

type Exp a b = Lan ((,) a) Identity b

toExp :: (a -> b) -> Exp a b
toExp f = Lan (f . fst) (Identity ())

fromExp :: Exp a b -> (a -> b)
fromExp (Lan f (Identity x)) = \a -> f (a, x)

main :: IO ()
main = do
    let exp' = fromExp (toExp (==1))
    putStrLn $ show $ exp' 1
    putStrLn $ show $ exp' 2
    return ()
