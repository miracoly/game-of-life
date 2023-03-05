module GameOfLife (module GameOfLife) where

import Data.List (nub, sort)

type Board = [Position]

data Position = Position
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq, Ord)
  
nextTicks :: Board -> Int -> Board
nextTicks b n 
  | n < 1 = b 
  | n == 1 = nextTick b
  | otherwise = nextTicks (nextTick b) (n -1)

nextTick :: Board -> Board
nextTick b = sort $ getSurvivors b ++ getBirths b

getSurvivors :: Board -> [Position]
getSurvivors b = filter (doesSurvive b) b

getBirths :: Board -> [Position]
getBirths b = filter (isToBeBorn b) $ filter (isDead b) $ nub $ b >>= getNeighbors

isToBeBorn :: Board -> Position -> Bool
isToBeBorn b = (==) 3 . getCountOfLifeNeighbors b

doesSurvive :: Board -> Position -> Bool
doesSurvive b p = getCountOfLifeNeighbors b p `elem` [2, 3]

isAlive :: Board -> Position -> Bool
isAlive b p = p `elem` b

isDead :: Board -> Position -> Bool
isDead b p = p `notElem` b

getCountOfLifeNeighbors :: Board -> Position -> Int
getCountOfLifeNeighbors b = length . filter (isAlive b) . getNeighbors

getNeighbors :: Position -> [Position]
getNeighbors (Position a b) =
  [ Position (a -1) (b -1),
    Position a (b - 1),
    Position (a + 1) (b - 1),
    Position (a - 1) b,
    Position (a + 1) b,
    Position (a - 1) (b + 1),
    Position a (b + 1),
    Position (a + 1) (b + 1)
  ]
