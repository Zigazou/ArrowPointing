{- |
Module      : Grid
Description : A `Grid` containing arrows to follow.
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
-}
module Grid.Parse (toGrid) where

import Data.Array (listArray)
import Data.List (transpose)
import Data.Maybe (fromJust)
import Control.Arrow ((&&&))

import Cell (Cell (Start), toCell)

import Grid.Type (Grid (Grid), arraySearch)

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
toGrid s = validateGrid s >> return (Grid start cells)
    where
        ls = lines s
        (width, height) = (length . head &&& length) ls
        cells = toCell <$> listArray ((0, 0), (width - 1, height - 1))
                                     (concat $ transpose ls)
        start = fromJust $ arraySearch cells Start
