module Main where

import           Data.List          (nub)
import           System.Environment (getArgs)

marker :: String -> Maybe String
marker s
  | length prefix < 4        = Nothing
  | length (nub prefix) == 4 = Just prefix
  | otherwise                = (c :) <$> marker cs
  where prefix = take 4 s
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
