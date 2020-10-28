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

store :: (s -> a) -> s -> Store s a
store f s = Store f s

-- 1-dimensional GoL: Cells with exactly one living neighbor change (alive cells become dead, dead cells become alive)

data Cell = Live | Dead deriving Show
type Position = Int
type Grid = Store Position Cell
type Stepper = Grid -> Grid

next Live Live Live = Live
next Dead Live Live = Dead
next Live Live Dead = Dead
next Dead Live Dead = Live
next Live Dead Live = Dead
next Dead Dead Live = Live
next Live Dead Dead = Live
next Dead Dead Dead = Dead

get :: Grid -> Position -> Cell
get (Store sa s) pos = sa (s + pos)

calc :: Grid -> Cell
calc grid =
  let
    current = extract grid
    left = get grid 1
    right = get grid (-1)
  in next left current right

step :: Stepper
step = extend calc

makeGrid :: [Position] -> Grid
makeGrid positions =
  let
    startingPosition = 0
    lookupCell pos = if elem pos positions then Live else Dead
  in store lookupCell startingPosition

makeIterations :: Stepper -> Grid -> [Grid]
makeIterations step grid =
  let nextGrid = step grid
  in nextGrid:makeIterations step nextGrid

toList :: Grid -> [Cell]
toList (Store sa _) = sa <$> [0..]

main = do
  let printWindow = 8
  let numIterations = 4
  let initialGrid = makeGrid [3, 4]
  let iterations = take numIterations $ makeIterations step initialGrid
  let printable = show $ take printWindow <$> toList <$> iterations
  putStrLn printable
  return ()
