{-# LANGUAGE TemplateHaskell #-}
{- |
Module      : testMain
Description : Tests for ArrowPointing
Copyright   : (c) Frédéric BISSON, 2015
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX
Tests for ArrowPointing
-}
module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import Data.Either (isLeft, isRight)
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, assert, run)

import Grid.Grid (toGrid, arrowPointing, prettyGridE)

-- Test valid files
validFiles :: [ValidFile]
validFiles =
    [ ValidFile "test/valid00.txt" "test/pretty-valid00.txt" 'b'
    , ValidFile "test/valid01.txt" "test/pretty-valid01.txt" 'Q'
    , ValidFile "test/valid02.txt" "test/pretty-valid02.txt" 'A'
    , ValidFile "test/valid03.txt" "test/pretty-valid03.txt" 'B'
    , ValidFile "test/valid04.txt" "test/pretty-valid04.txt" 'Y'
    , ValidFile "test/valid05.txt" "test/pretty-valid05.txt" 'B'
    , ValidFile "test/valid06.txt" "test/pretty-valid06.txt" 'E'
    ]

data ValidFile = ValidFile String String Char deriving Show
instance Arbitrary ValidFile where
    arbitrary = elements validFiles

prop_validGrid :: ValidFile -> Property
prop_validGrid (ValidFile fp _ _) = monadicIO $ do
    source <- run $ readFile fp
    assert $ isRight (toGrid source)

-- Tests pretty function
prop_pretty :: ValidFile -> Property
prop_pretty (ValidFile fs fp _) = monadicIO $ do
    source <- run $ readFile fs
    pretty <- run $ readFile fp
    assert $ prettyGridE (toGrid source) == (unlines . lines) pretty

-- Test solutions to grids
prop_validAnswers :: ValidFile -> Property
prop_validAnswers (ValidFile fp _ v) = monadicIO $ do
    source <- run $ readFile fp
    case toGrid source of
        Left _     -> assert False
        Right grid -> assert $ arrowPointing grid == Just v

-- Tests invalid grids
invalidFiles :: [String]
invalidFiles = [ "test/invalid00.txt"
               , "test/invalid01.txt"
               , "test/invalid02.txt"
               ]

newtype InvalidFile = InvalidFile String deriving Show
instance Arbitrary InvalidFile where
    arbitrary = elements $ InvalidFile <$> invalidFiles

prop_invalid :: InvalidFile -> Property
prop_invalid (InvalidFile fp) = monadicIO $ do
    source <- run $ readFile fp
    assert $ isLeft (toGrid source)

-- Helps TemplateHaskell work...
return []

main :: IO ()
main = do
    allPass <- $quickCheckAll
    unless allPass exitFailure
