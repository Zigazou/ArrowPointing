{- |
Module      : Grid
Description : A `Grid` containing arrows to follow.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Grid.Path (arrowPointing) where

import Data.List ((\\))
import Data.Maybe (listToMaybe, catMaybes)

import Cell ( Cell (Start, Line, Arrow, Value, Crossroad)
            , Direction (GoUp, GoRight, GoDown, GoLeft)
            , Axis (Horizontal, Vertical)
            )

import Grid.Type (Grid (gStart), Coordinate, at, neighbours)

{- |
Find neighbours around a `Cell` which could make a valid path.
-}
nexts :: Grid -> [Coordinate] -> Coordinate -> [Coordinate]
nexts g ps c = filter (validPath g c) (neighbours g c) \\ ps

{- |
Determine if two `Cell`s in a specific order could make a valid path. This
function is an helper used by `validPath`.
-}
validPath' :: Cell -- ^ Current cell
           -> Cell -- ^ Next cell
           -> Int  -- ^ Difference between x’s of the two cells
           -> Int  -- ^ Difference between y’s of the two cells
           -> Bool
-- Starting point
validPath' Start (Line Horizontal) x 0 = x == -1 || x == 1
validPath' Start (Line Vertical  ) 0 y = y == -1 || y == 1

-- Vertical line
validPath' (Line Vertical) (Line Vertical) 0    y = y == -1 || y == 1
validPath' (Line Vertical) (Arrow a      ) 0 (-1) = a /= GoDown
validPath' (Line Vertical) (Arrow a      ) 0    1 = a /= GoUp
validPath' (Line Vertical) Crossroad       0    y = y == -1 || y == 1

-- Horizontal line
validPath' (Line Horizontal) (Line Horizontal)    x 0 = x == -1 || x == 1
validPath' (Line Horizontal) (Arrow a        ) (-1) 0 = a /= GoRight
validPath' (Line Horizontal) (Arrow a        )    1 0 = a /= GoLeft
validPath' (Line Horizontal) Crossroad            x 0 = x == -1 || x == 1

-- Crossroad
validPath' (Crossroad) (Line Horizontal) x 0 = x == -1 || x == 1
validPath' (Crossroad) (Line Vertical  ) 0 y = y == -1 || y == 1
validPath' (Crossroad) Crossroad         _ _ = True

-- Arrow
validPath' (Arrow GoUp   ) (Value _)    0 (-1) = True
validPath' (Arrow GoDown ) (Value _)    0    1 = True
validPath' (Arrow GoLeft ) (Value _) (-1)    0 = True
validPath' (Arrow GoRight) (Value _)    1    0 = True

-- Everything else is invalid
validPath' _ _ _ _ = False

{- |
Determine if two `Cell`s in a specific order could make a valid path.
-}
validPath :: Grid -> Coordinate -> Coordinate -> Bool
validPath g c1@(x1, y1) c2@(x2, y2) =
    validPath' (g `at` c1) (g `at` c2) (x2 - x1) (y2 - y1)

{- |
Search for a `Char` pointed to by an arrow. This is an helper function for the
`arrowPointing` function.
-}
arrowPointing' :: Grid         -- ^ The `Grid`
               -> [Coordinate] -- ^ `Coordinate`s already visited
               -> Coordinate   -- ^ `Coordinate` to visit
               -> Maybe Char   -- ^ `Char`acter found
arrowPointing' g ps c = case g `at` c of
    Value v -> Just v
    _ -> (listToMaybe . catMaybes) $ arrowPointing' g (c:ps) <$> nexts g ps c

{- |
Search for a `Char` pointed to by an arrow.
-}
arrowPointing :: Grid -> Maybe Char
arrowPointing g = arrowPointing' g [] (gStart g)
