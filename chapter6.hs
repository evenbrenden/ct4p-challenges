
-- 1

iso1 :: Maybe a -> Either a ()
iso1 (Just a) = Left a
iso1 Nothing = Right ()

inv1 :: Either a () -> Maybe a
inv1 (Left a) = Just a
inv1 (Right _) = Nothing

-- 2-4

data Shape = Circle Float | Rect Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square d) = d * d

circ :: Shape -> Float
circ (Circle r) = 2 * pi * r
circ (Rect d h) = 2 * pi * (d + h)
circ (Square d) = 2 * pi * (d + d)

-- 6

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

-- main

main :: IO ()
main =
  return ()
