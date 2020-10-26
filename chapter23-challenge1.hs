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

-- Game of Life

type Grid = [Bool]

get index grid = grid !! (mod index $ length grid)

next True True True = True
next False True True = False
next True True False = False
next False True False = True
next True False True = False
next False False True = True
next True False False = True
next False False False = False

nextValue grid i = next (get (i-1) grid) (get i grid) (get (i+1) grid)

gridStore :: Grid -> Store Int Bool
gridStore grid = Store (\i -> nextValue grid i) 0

iterations :: Grid -> [Grid]
iterations state =
  let store = gridStore state
      positions i = [i..(length state)-1]
      nextIteration = experiment positions store
  in nextIteration:(iterations nextIteration)

initial :: Grid
initial = [False, True, True, False, False, True, False, True]

fourFirstIterations = take 4 $ iterations initial

main = do
  putStrLn $ show $ fourFirstIterations
  return ()
