module Main where

import           Data.Char          (isUpper, ord, toLower)
import           Data.List.Split    (chunksOf)
import           System.Environment (getArgs)

uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (a:as) | a `elem` as = uniq as
            | otherwise = a:as

duplicates :: (String, String, String) -> String
duplicates (s1, s2, s3) = filter dup s1
  where dup c = c `elem` s2 && c `elem` s3

priority :: Char -> Int
priority c | isUpper c = 26 + priority (toLower c)
           | otherwise = ord c - ord 'a' + 1

toGroup :: [String] -> (String, String, String)
toGroup [s1, s2, s3] = (s1, s2, s3)
toGroup ls           = error $ "incomplete input: " ++ show ls

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let groups = map toGroup $ chunksOf 3 $ lines cts
      badges = map (uniq . duplicates) groups

  print $ sum $ map priority $ concat badges
