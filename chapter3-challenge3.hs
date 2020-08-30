-- AND monoid
newtype BoolConj =
  BoolConj Bool
  deriving (Eq, Show)

-- <> is mappend
instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True

-- Semigroup + mempty = Monoid
instance Monoid BoolConj where
  mempty = BoolConj True

-- OR monoid
newtype BoolDisj =
  BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

main :: IO ()
main = return ()
