-- Store comonad

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Store s a = Store (s -> a) s

instance Functor (Store s) where
  fmap f (Store sa s) = Store (f . sa) s

instance Comonad (Store s) where
  extract (Store f s) = f s
  duplicate (Store f s) = Store (Store f) s

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f <$> k s

runStore :: Store s a -> a
runStore (Store sa s) = sa s

-- Stream comonad

data Stream a = Cons a (Stream a) deriving Show

instance Functor Stream where
  fmap f (Cons a s) = Cons (f a) (fmap f s)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)

head' :: Stream a -> a
head' = extract

tail' :: Stream a -> Stream a
tail' (Cons _ s) = s

get :: Int -> Stream a -> a
get pos s = if pos == 0 then (head' s) else get (pos - 1) (tail' s)

-- Game of Life

data Cell = Live | Dead deriving Show

type Grid = Stream Cell

next Live Live Live = Live
next Dead Live Live = Dead
next Live Live Dead = Dead
next Dead Live Dead = Live
next Live Dead Live = Dead
next Dead Dead Live = Live
next Live Dead Dead = Live
next Dead Dead Dead = Dead

nextValue grid 0 = next Dead (get 1 grid) (get 2 grid) -- unidirectional so wtd
nextValue grid pos = next (get (pos - 1) grid) (get pos grid) (get (pos + 1) grid)

gridStore :: Grid -> Store Int Cell
gridStore grid =
  let startPos = 0
  in Store (\i -> nextValue grid i) startPos

monotonicallyIncreasing :: Int -> Stream Int
monotonicallyIncreasing a = Cons a (monotonicallyIncreasing (a + 1))

iterations :: Grid -> [Grid]
iterations grid =
  let store = gridStore grid
      positions = \pos -> monotonicallyIncreasing pos
      nextIteration = experiment positions store
  in nextIteration:(iterations nextIteration)

allDead :: Grid
allDead = Cons Dead allDead

initial :: Grid
initial = Cons Live allDead

it1 = head $ iterations initial
it2 = head $ drop 1 $ iterations initial
it3 = head $ drop 2 $ iterations initial
it4 = head $ drop 3 $ iterations initial

toList :: Int -> Stream a -> [a]
toList n (Cons a s) = if n == 1 then [a] else a:(toList (n-1) s)

main = do
  putStrLn $ "Initial state:\t" ++ (show $ toList 8 initial)
  putStrLn $ "1st iteration:\t" ++ (show $ toList 8 it1)
  putStrLn $ "2nd iteration:\t" ++ (show $ toList 8 it2)
  putStrLn $ "3rd iteration:\t" ++ (show $ toList 8 it3)
  putStrLn $ "4th iteration:\t" ++ (show $ toList 8 it4)
  return ()
