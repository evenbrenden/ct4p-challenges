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

data Cell = Live | Dead deriving (Eq, Show)
type Position = (Int, Int)
type Grid = Store Position Cell
type Stepper = Grid -> Grid

calc :: Grid -> Cell
calc grid =
  let current = extract grid
      relativeNeighbourPositions = [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1], (x, y) /= (0, 0)]
      shiftPositions (x, y) (shiftX, shiftY) = (x + shiftX, y + shiftY)
      absoluteNeighbourPositions position = shiftPositions position <$> relativeNeighbourPositions
      neighbours = experiment absoluteNeighbourPositions grid
      numNeighboursAlive = length (filter (== Live) neighbours)
  in if (current == Live && (numNeighboursAlive == 2 || numNeighboursAlive == 3)) ||
        (current == Dead && numNeighboursAlive == 3)
      then Live
      else Dead

step :: Stepper
step = extend calc

makeGrid :: [Position] -> Grid
makeGrid livePositions =
  let startingPosition = (0, 0)
      lookupCell position = if elem position livePositions then Live else Dead
  in Store lookupCell startingPosition

makeIterations :: Stepper -> Grid -> [Grid]
makeIterations step grid = grid:makeIterations step (step grid)

toString :: Int -> Grid -> [String]
toString window (Store sa _) =
  let view = [(y, x) | x <- [0..window - 1], y <- [0..window - 1]]
      list = sa <$> view
      rows = chunksOf window list
      render cells = concat $ show <$> cells
      addNewLine string = string ++ "\n"
  in addNewLine . render <$> rows

main = do
  let printWindow = 3
  let numIterations = 3
  let initialGrid = makeGrid [(0, 1), (1, 1), (2, 1)]
  let iterations = take numIterations $ makeIterations step initialGrid
  let printable = toString printWindow <$> iterations
  putStrLn "------------"
  mapM_ putStrLn $ head printable
  putStrLn "------------"
  mapM_ putStrLn $ head $ drop 1 $ printable
  putStrLn "------------"
  mapM_ putStrLn $ head $ drop 2 $ printable
  return ()
