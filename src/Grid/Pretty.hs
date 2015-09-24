{- |
Module      : Grid
Description : A `Grid` containing arrows to follow.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Grid.Pretty (prettyGrid, prettyGridE) where

import Data.Array (indices)
import Data.List (transpose)
import Data.List.Split (chunksOf)

import Cell ( Cell (Start, Line, Arrow, Value, Crossroad, Empty)
            , Direction (GoUp, GoRight, GoDown, GoLeft)
            , Axis (Horizontal, Vertical)
            )

import Grid.Type (Grid (gCells), at, inside, gHeight, Coordinate)

{- |
Transform a `Crossroad` into a Unicode character given its branches.
-}
prettyCross :: Bool -- ^ Branch Up
            -> Bool -- ^ Branch Right
            -> Bool -- ^ Branch Down
            -> Bool -- ^ Branch Left
            -> Char
-- 4 branches
prettyCross True  True  True  True  = '┼'

-- 3 branches
prettyCross False True  True  True  = '┬'
prettyCross True  False True  True  = '┤'
prettyCross True  True  False True  = '┴'
prettyCross True  True  True  False = '├'

-- 2 branches
prettyCross True  False True  False = '│'
prettyCross False True  False True  = '─'
prettyCross True  True  False False = '└'
prettyCross False True  True  False = '┌'
prettyCross False False True  True  = '┐'
prettyCross True  False False True  = '┘'

-- 1 branch
prettyCross True  False False False = '╵'
prettyCross False True  False False = '╶'
prettyCross False False True  False = '╷'
prettyCross False False False True  = '╴'

-- No branch!
prettyCross False False False False = '▪'

{- |
Give a Unicode character for a `Cell` based on its direct neighbours. Neighbours
must be given in the following order: up, right, down and left
-}
prettyCell' :: Cell -> [Cell] -> Char
prettyCell' Crossroad [up, ri, dn, le] = 
    prettyCross (up == Line Vertical   || up == Crossroad)
                (ri == Line Horizontal || ri == Crossroad)
                (dn == Line Vertical   || dn == Crossroad)
                (le == Line Horizontal || le == Crossroad)
prettyCell' Start             _ = '●'
prettyCell' (Line Horizontal) _ = '─'
prettyCell' (Line Vertical)   _ = '│'
prettyCell' (Arrow GoUp)      _ = '▲'
prettyCell' (Arrow GoRight)   _ = '▶'
prettyCell' (Arrow GoDown)    _ = '▼'
prettyCell' (Arrow GoLeft)    _ = '◀'
prettyCell' (Value c)         _ = c
prettyCell' _                 _ = ' '

{- |
Use Unicode chars to draw a better `Cell`.
-}
prettyCell :: Grid -> Coordinate -> Char
prettyCell g c@(x,y) = prettyCell' (g `at` c) around
    where around = cell <$> [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
          cell c' = if c' `inside` g then g `at` c'
                                     else Empty

{- |
Pretty print a `Grid`
-}
prettyGrid :: Grid -> String
prettyGrid g = unlines $  transpose
               (chunksOf (gHeight g) (prettyCell g <$> (indices . gCells) g))

{- |
Make it easier to work with output of the `toGrid` function.
-}
prettyGridE :: Either String Grid -> String
prettyGridE = either id prettyGrid
