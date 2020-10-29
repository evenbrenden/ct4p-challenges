-- Store comonad (from Control.Comonad.Store)

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

-- Helpers (from Data.List.Split)

build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- Game of Life

data State = Live | Dead deriving (Eq, Show)
type Position = (Int, Int)
type Grid = Store Position State
type Stepper = Grid -> Grid

relativeNeighbourPositions :: [Position]
relativeNeighbourPositions = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

move :: Position -> Position -> Position
move (x, y) (shiftX, shiftY) = (x + shiftX, y + shiftY)

neighbourPositions :: Position -> [Position]
neighbourPositions position = move position <$> relativeNeighbourPositions

neighbourStates :: Grid -> [State]
neighbourStates grid = experiment neighbourPositions grid

nextState :: Grid -> State
nextState grid =
  let currentState = extract grid
      numNeighboursAlive = length (filter (== Live) (neighbourStates grid))
  in if (currentState == Live && (numNeighboursAlive == 2 || numNeighboursAlive == 3)) ||
        (currentState == Dead && numNeighboursAlive == 3)
      then Live
      else Dead

step :: Stepper
step = extend nextState

makeGrid :: [Position] -> Grid
makeGrid livePositions =
  let startingPosition = (0, 0)
      lookupState position =
        if elem position livePositions
        then Live
        else Dead
  in Store lookupState startingPosition

makeIterations :: Stepper -> Grid -> [Grid]
makeIterations step grid = grid:makeIterations step (step grid)

toString :: Int -> Grid -> String
toString window (Store sa _) =
  let view = [(x, y) | y <- [0..window - 1], x <- [0..window - 1]]
      selectedCells = sa <$> view
      rowsOfCells = chunksOf window $ selectedCells
      render cells = concat $ show <$> cells
      renderedRows = unlines $ render <$> rowsOfCells
  in renderedRows

main = do
  let printWindow = 3 -- Print a 3x3 grid
  let numIterations = 3 -- Print 3 iterations
  let seed = makeGrid [(0, 1), (1, 1), (2, 1)] -- Seed is a "blinker"
  let iterations = take numIterations $ makeIterations step seed
  let printable = toString printWindow <$> iterations
  mapM_ putStrLn printable
  return ()
