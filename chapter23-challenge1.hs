-- Store comonad

class Functor w => Comonad w where
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  extract :: w a -> a

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

-- Stream functor

data Stream a = Cons a (Stream a) (Stream a) deriving Show

instance Functor Stream where
  fmap f (Cons a sl sr) = Cons (f a) (fmap f sl) (fmap f sr)

head' :: Stream a -> a
head' (Cons a _ _ ) = a

fwd :: Stream a -> Stream a
fwd (Cons _ _ sr) = sr

bwd :: Stream a -> Stream a
bwd (Cons _ sl _) = sl

get :: Int -> Stream a -> a
get pos s =
  if pos == 0
    then (head' s)
  else if pos > 0
    then get (pos - 1) (fwd s)
  else get (pos + 1) (bwd s)

monotonicallyIncreasing :: Int -> Stream Int
monotonicallyIncreasing a = Cons a (monotonicallyIncreasing (a - 1)) (monotonicallyIncreasing (a + 1))

-- 1-dimensional GoL: Cells with exactly one living neighbor change (alive cells become dead, dead cells become alive)

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

nextValue grid pos = next (get (pos - 1) grid) (get pos grid) (get (pos + 1) grid)
gridStore :: Grid -> Store Int Cell
gridStore grid =
  let startPos = 0
  in Store (\i -> nextValue grid i) startPos

iterations :: Grid -> [Grid]
iterations grid =
  let store = gridStore grid
      positions = \pos -> monotonicallyIncreasing pos
      nextIteration = experiment positions store
  in nextIteration:(iterations nextIteration)

allDead :: Grid
allDead = Cons Dead allDead allDead

initial :: Grid
initial = Cons Dead allDead (Cons Dead allDead (Cons Dead allDead (Cons Live allDead (Cons Live allDead allDead))))

it1 = head $ iterations initial
it2 = head $ drop 1 $ iterations initial
it3 = head $ drop 2 $ iterations initial
it4 = head $ drop 3 $ iterations initial

toListFwd :: Int -> Stream a -> [a]
toListFwd n (Cons a sr sl) = if n == 1 then [a] else a:(toListFwd (n - 1) sl)

main = do
  putStrLn $ "Initial state:\t" ++ (show $ toListFwd 8 initial)
  putStrLn $ "1st iteration:\t" ++ (show $ toListFwd 8 it1)
  putStrLn $ "2nd iteration:\t" ++ (show $ toListFwd 8 it2)
  putStrLn $ "3rd iteration:\t" ++ (show $ toListFwd 8 it3)
  putStrLn $ "4th iteration:\t" ++ (show $ toListFwd 8 it4)
  return ()
