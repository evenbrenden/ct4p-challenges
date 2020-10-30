#! /usr/bin/env nix-shell
#! nix-shell -p ghcid
#! nix-shell -p "haskellPackages.ghcWithPackages (p: [p.split p.utility-ht p.MemoTrie])"
#! nix-shell -i "ghcid -c 'ghci -Wall' -T main"

import Data.List.Split -- For chunksOf
import Data.List.HT -- For range
import Data.MemoTrie -- For memoization

-- Store comonad

class Functor w => Comonad w where
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  extract :: w a -> a

data Store s a = Store (s -> a) s

instance HasTrie s => Functor (Store s) where
  fmap f (Store sa s) = Store (memo $ f . sa) s

instance HasTrie s => Comonad (Store s) where
  extract (Store sa s) = sa s
  duplicate (Store sa s) = Store (Store sa) s

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment k (Store f s) = f <$> k s

-- Game of Life

data State = Live | Dead deriving (Eq, Show)
type Position = (Int, Int)
type Grid = Store Position State
type Stepper = Grid -> Grid

relativeNeighbourPositions :: [Position]
relativeNeighbourPositions = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

move :: Position -> Position -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

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
makeIterations step' grid = grid:makeIterations step' (step' grid)

toString :: Int -> Grid -> String
toString window (Store sa _) =
  let viewPositions = [(x, y) | y <- range window, x <- range window]
      selectedCells = sa <$> viewPositions
      rowsOfCells = chunksOf window $ selectedCells
      render cells = concat $ show <$> cells
      renderedRows = unlines $ render <$> rowsOfCells
  in renderedRows

main :: IO ()
main = do
  let printWindow = 3 -- Print a 3x3 grid
  let numIterations = 10 -- Print 3 iterations
  let seed = makeGrid [(0, 1), (1, 1), (2, 1)] -- "Blinker" seed
  let iterations = take numIterations $ makeIterations step seed
  let printables = toString printWindow <$> iterations
  mapM_ putStrLn printables
  return ()
