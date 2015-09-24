module Main where

import System.Directory (getDirectoryContents)
import Data.String.Utils (startswith, endswith)
import Data.List (sort)

import Grid.Grid (Grid, toGrid, arrowPointing, prettyGrid)

{- |
Try to find where the arrow is pointing and prints information about the grid.
-}
processGrid :: (String, Either String Grid) -> IO ()
processGrid (filename, Left msg) = putStrLn $ filename ++ ": " ++ msg
processGrid (filename, Right grid) = do
    putStrLn filename
    putStr $ prettyGrid grid
    case arrowPointing grid of
        Just v  -> putStrLn ("Solution=" ++ [v])
        Nothing -> putStrLn "No solution!"
    putStrLn ""

{- |
Find valid files in the test directory. File names must start with "valid" and
end with ".txt".
-}
findValidFiles :: IO [FilePath]
findValidFiles = do
    filenames <- getDirectoryContents "test"
    return $ sort $ ("test/" ++) <$> filter validFile filenames
    where validFile fp = startswith "valid" fp && endswith ".txt" fp

main :: IO ()
main = do
    validFiles <- findValidFiles
    sources <- sequence $ readFile <$> validFiles
    mapM_ processGrid (zip validFiles (toGrid <$> sources))
