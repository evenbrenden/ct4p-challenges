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

and' :: Bool -> Bool
and' = (&&) True

memo :: Pair Bool
memo = tabulate and'

memos :: (Bool, Bool)
memos = fmap (index memo) (False, True)

main = do
  putStrLn $ show memos
  return ()
