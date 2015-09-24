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

import Cell (Cell (Start), toCell)

import Grid.Type (Grid (Grid), arraySearch)

{- |
Verify a `String` contains a valid problem.
-}
validateGrid :: String -> Either String String
validateGrid s
    | null s                          = Left "Empty string"
    | startCounts s == 0              = Left "No start point"
    | startCounts s > 1               = Left "More than one start point"
    | otherwise                       = Right s
    where startCounts = length . filter (== 'S')

{- |
Limit a `String` at a specific length. If the source `String` is less than the
targetted length, it is padded with spaces.
-}
limit :: Int -> String -> String
limit i s | l >= i = take i s 
          | otherwise = s ++ replicate (i - l) ' '
          where l = length s

{- |
Convert a `String` into a `Grid`.
-}
toGrid :: String -> Either String Grid
toGrid s = validateGrid s >> return (Grid start cells)
    where
        ls = lines s
        (width, height) = (maximum (length <$> ls), length ls)
        cells = toCell <$> listArray ((0, 0), (width - 1, height - 1))
                                     (concat $ transpose (limit width <$> ls))
        start = fromJust $ arraySearch cells Start
