module TestData (module TestData) where

import GameOfLife (Board, Position (..))

{-
    x - - x -
    x - - x -
    - - - - -
    - - x - -
    - x - - -
-}
underpopulation :: Board
underpopulation =
  [ Position 1 1,
    Position 4 1,
    Position 1 2,
    Position 4 2,
    Position 3 4,
    Position 2 5
  ]

{-
    - - - - -
    - x x - -
    x - - x -
    - x x - -
    - - - - -
-}
static :: Board
static =
  [ Position 2 2,
    Position 3 2,
    Position 1 3,
    Position 4 3,
    Position 2 4,
    Position 3 4
  ]

{-
    - - - - -
    - - x - -
    - x x x -
    - - x - -
    - - - - -
-}
overpopulation :: Board
overpopulation =
  [ Position 3 2,
    Position 2 3,
    Position 3 3,
    Position 4 3,
    Position 3 4
  ]

{-
    - - - - -
    - x x x -
    - x - x -
    - x x x -
    - - - - -
-}
nextTickOverpopulation :: Board
nextTickOverpopulation =
  [ Position 3 2,
    Position 2 3,
    Position 4 3,
    Position 3 4,
    Position 2 2,
    Position 4 2,
    Position 2 4,
    Position 4 4
  ]

{-
    - - - - -
    - - x - -
    - x - - -
    - - - x -
    - - - - -
-}
birth :: Board
birth =
  [ Position 3 2,
    Position 2 3,
    Position 4 4
  ]

{-
    - - - - -
    - - - - -
    - - x - -
    - - - - -
    - - - - -
-}
nextTickBirth :: Board
nextTickBirth = [ Position 3 3 ]