module Main where

import Data.Either (rights)
import Control.Monad (forM_)

import Pretty (pretty)
import Grid (toGrid, arrowPointing)

validFiles :: [String]
validFiles = [ "test/valid00.txt"
             , "test/valid01.txt"
             , "test/valid02.txt"
             , "test/valid03.txt"
             , "test/valid04.txt"
             ]

main :: IO ()
main = do
    sources <- sequence $ readFile <$> validFiles
    let grids = rights $ toGrid <$> sources
    forM_ grids $ \grid -> do
        let prettyGrid = pretty grid
        print $ length prettyGrid
        putStr prettyGrid
        case arrowPointing grid of
            Just v -> putStrLn $ "Solution=" ++ [v]
            Nothing -> putStrLn "No solution!"
        putStrLn ""

{-    
main :: IO ()
main = do
    source <- readFile "test/valid00.txt"
    let eGrid = toGrid source
        grid = either (\_ -> error "!!!") id eGrid
    (putStrLn . pretty) grid
    (print . arrowPointing) grid
    -}