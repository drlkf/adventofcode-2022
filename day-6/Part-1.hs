module Main where

import           Data.List          (nub)
import           System.Environment (getArgs)

prefixLength :: Int
prefixLength = 4 -- part 1
-- prefixLength = 14 -- part 2

marker :: String -> Maybe String
marker s
  | length prefix < prefixLength        = Nothing
  | length (nub prefix) == prefixLength = Just prefix
  | otherwise                           = (c :) <$> marker cs
  where prefix = take prefixLength s
        c = head s
        cs = tail s

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  maybe (pure ()) (print . length) $ marker cts
