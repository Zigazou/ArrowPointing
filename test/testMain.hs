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
import Test.QuickCheck.Monadic

import Pretty (pretty)
import Grid (toGrid, arrowPointing)

-- Test valid files
validFiles :: [String]
validFiles = [ "test/valid00.txt"
             , "test/valid01.txt"
             , "test/valid02.txt"
             , "test/valid03.txt"
             , "test/valid04.txt"
             ]

newtype ValidFile = ValidFile String deriving Show
instance Arbitrary ValidFile where
    arbitrary = elements $ ValidFile <$> validFiles

prop_validGrid :: ValidFile -> Property
prop_validGrid (ValidFile fp) = monadicIO $ do
    source <- run $ readFile fp
    assert $ isRight (toGrid source)

-- Test solutions to grids
validAnswers :: [Maybe Char]
validAnswers = [ Just 'b'
               , Just 'Q'
               , Just 'A'
               , Just 'B'
               , Just 'Y'
               ]

newtype ValidAnswer = ValidAnswer (String, Maybe Char) deriving Show
instance Arbitrary ValidAnswer where
    arbitrary = elements $ ValidAnswer <$> zip validFiles validAnswers

prop_validAnswers :: ValidAnswer -> Property
prop_validAnswers (ValidAnswer (fp, v)) = monadicIO $ do
    source <- run $ readFile fp
    case toGrid source of
        Left _     -> assert False
        Right grid -> assert $ arrowPointing grid == v

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
