module Main where

import           Data.Char          (isUpper, ord, toLower)
import           System.Environment (getArgs)

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (a:as) | a `elem` as = uniq as
            | otherwise = a:as

duplicates :: (String, String) -> String
duplicates (s1, s2) = filter (`elem` s2) s1

splitHalf :: String -> (String, String)
splitHalf s = (s1, s2)
  where n = length s `div` 2
        s1 = take n s
        s2 = drop n s

priority :: Char -> Int
priority c | isUpper c = 26 + priority (toLower c)
           | otherwise = ord c - ord 'a' + 1

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let bags = map splitHalf $ lines cts
      dups = map (uniq . duplicates) bags
      priorities = map (map priority) dups

  print $ sum $ map sum priorities
