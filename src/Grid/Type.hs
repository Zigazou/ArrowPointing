{- |
Module      : Type
Description : A `Grid` containing arrows to follow.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Grid.Type
( Coordinate
, Grid(Grid, gStart, gCells)
, gWidth
, gHeight
, inside
, at
, arraySearch
, neighbours
) where

import Data.Array (Array, assocs, inRange, bounds, (!))
import Data.List (find)

import Cell (Cell)

type Coordinate = (Int, Int)

data Grid = Grid { gStart :: Coordinate, gCells  :: Array Coordinate Cell }

gWidth :: Grid -> Int
gWidth = (1 +) . fst . snd . bounds . gCells

gHeight :: Grid -> Int
gHeight = (1 +) . snd . snd . bounds . gCells

{- |
Verify that a `Coordinate` is valid for a `Grid`.
-}
inside :: Coordinate -> Grid -> Bool
inside c g = inRange (bounds $ gCells g) c

{- |
Return the `Cell` contained at specific `Coordinate`s in a `Grid`.
-}
at :: Grid -> Coordinate -> Cell
at g c = gCells g ! c

{- |
Search for a specific `Cell` in an `Array`.
-}
arraySearch :: Array Coordinate Cell -> Cell -> Maybe Coordinate
arraySearch a cell = fst <$> find ((==) cell . snd) (assocs a)

{- |
Returns the neighbours of a `Cell` in a `Grid`.
-}
neighbours :: Grid -> Coordinate -> [Coordinate]
neighbours g (x, y) =
    filter (`inside` g) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
