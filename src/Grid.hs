{-# LANGUAGE FlexibleInstances #-}
{- |
Module      : Grid
Description : A `Grid` containing arrows to follow.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Grid
( toGrid
, Coordinate
, Grid(gWidth, gHeight, gStart, gCells)
, arrowPointing
) where

import Data.Array (Array, listArray, assocs, elems, inRange, bounds, (!))
import Data.List (find, (\\), transpose)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import Control.Arrow ((&&&))

import Cell ( Cell (Start, Line, Arrow, Value, Crossroad)
            , Direction (GoUp, GoRight, GoDown, GoLeft)
            , Axis (Horizontal, Vertical)
            , toCell
            )

import Pretty (Pretty, pretty)

type Coordinate = (Int, Int)

data Grid = Grid { gWidth  :: Int
                 , gHeight :: Int
                 , gStart  :: Coordinate
                 , gCells  :: Array Coordinate Cell
                 }

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
Verify a `String` contains a valid problem.
-}
validateGrid :: String -> Either String String
validateGrid s
    | null s                          = Left "Empty string"
    | any ((/= width ls) . length) ls = Left "Rows with different lengths"
    | startCounts s == 0              = Left "No start point"
    | startCounts s > 1               = Left "More than one start point"
    | otherwise                       = Right s
    where ls = lines s
          width = length . head
          startCounts = length . filter (== 'S')

{- |
Convert a `String` into a `Grid`.
-}
toGrid :: String -> Either String Grid
toGrid s = validateGrid s >> return (Grid width height start cells)
    where
        ls = lines s
        (width, height) = (length . head &&& length) ls
        cells = toCell <$> listArray ((0, 0), (width - 1, height - 1))
                                     (concat $ transpose ls)
        start = fromJust $ arraySearch cells Start

{- |
Returns the neighbours of a `Cell` in a `Grid`.
-}
neighbours :: Grid -> Coordinate -> [Coordinate]
neighbours g (x, y) =
    filter (`inside` g) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

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

{- |
Instance for `Grid`.
-}
instance Pretty Grid where
    pretty g = unlines $ concat <$> pretty
             <€> transpose (chunksOf (gHeight g) (elems $ gCells g))
            where (<€>) = (<$>) . (<$>)

{- |
Make it easier to work with output of the `toGrid` function.
-}
instance Pretty (Either String Grid) where
    pretty = either id pretty