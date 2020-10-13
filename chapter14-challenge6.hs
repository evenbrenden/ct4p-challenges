{-# LANGUAGE TypeFamilies #-}

class Representable f where
  type Rep f :: *
  tabulate :: (Rep f -> x) -> f x
  index :: f x -> Rep f -> x

data Pair a = Pair a a deriving Show

instance Representable Pair where
  type Rep Pair = Bool
  tabulate f = Pair (f True) (f False)
  index (Pair x _) True = x
  index (Pair _ y) False = y

toBeMemoized :: Bool -> Int
toBeMemoized True = 40
toBeMemoized False = 21

memo :: Pair Int
memo = tabulate toBeMemoized

memos :: (Int, Int)
memos = (index memo False, index memo True)

main = do
  putStrLn $ show memos
  return ()
