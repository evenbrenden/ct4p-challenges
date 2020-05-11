data Shape = Circle Float | Rect Float Float | Square Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square d) = d * d

circ :: Shape -> Float
circ (Circle r) = 2 * pi * r
circ (Rect d h) = 2 * pi * (d + h)
circ (Square d) = 2 * pi * (d + d)

main :: IO ()
main =
  return ()
