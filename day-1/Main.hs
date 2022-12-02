module Main where

import           Data.List.Split    (splitOn)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let elves = map (map read) $ splitOn [""] $ lines cts :: [[Int]]
      totals = map sum elves

  print $ maximum totals
