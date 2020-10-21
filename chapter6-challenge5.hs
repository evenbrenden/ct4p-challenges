-- Numbers         Types
-- 2 = 1 + 1       data Bool = True | False
-- a * b           (a, b)
-- a + a           Either a a = Left a | Right a
-- 2 * a           (Bool, a)

iso5 :: Either a a -> (Bool, a)
iso5 (Left a) -> (True, a)
iso5 (Right a) -> (False, a)

inv5 :: (Bool, a) -> Either a a
inv5 (True, a) -> Left a
inv5 (False, a) -> Right a
