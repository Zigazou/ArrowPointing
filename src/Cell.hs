{- |
Module      : Cell
Description : A `Cell` in a `Grid`.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Cell
( Direction (..)
, Axis (..)
, Cell (..)
, toCell
) where

{- |
Direction an `Arrow` may take.
-}
data Direction = GoUp | GoRight | GoDown | GoLeft deriving (Eq, Show)

{- |
Axis of a `Line`.
-}
data Axis = Horizontal | Vertical deriving (Eq, Show)

{- |
A `Cell` may contain 6 kinds of value.
-}
data Cell = Start           -- ^ A starting point
          | Line Axis       -- ^ An horizontal or vertical line
          | Crossroad       -- ^ An axis changer
          | Arrow Direction -- ^ An arrow pointing to a direction
          | Value Char      -- ^ A character
          | Empty           -- ^ An empty cell
          deriving (Eq, Show)

{- |
Convert a `Char` to a `Cell`.
-}
toCell :: Char -> Cell
toCell 'S' = Start
toCell '-' = Line Horizontal
toCell '|' = Line Vertical
toCell '+' = Crossroad
toCell '>' = Arrow GoRight
toCell '<' = Arrow GoLeft
toCell '^' = Arrow GoUp
toCell 'V' = Arrow GoDown
toCell ' ' = Empty
toCell c   = Value c
