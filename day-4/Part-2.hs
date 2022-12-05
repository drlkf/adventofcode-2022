module Main where

import           Data.Char                       (isDigit)
import           System.Environment              (getArgs)
import           Text.ParserCombinators.ReadP    (many, satisfy)
import           Text.ParserCombinators.ReadPrec (get, lift, pfail)
import           Text.Read                       (readPrec)

data Assignment = Assignment Int Int
  deriving Show

instance Read Assignment where
  readPrec = do
    x <- number
    get >>= dash
    y <- number
    return $ Assignment (read x) (read y)
      where dash '-' = return ()
            dash  _  = pfail
            number = lift $ many $ satisfy isDigit

data ElfPair = ElfPair Assignment Assignment
  deriving Show

instance Read ElfPair where
  readPrec = do
    left <- readPrec
    get >>= comma
    right <- readPrec
    return $ ElfPair left right
    where comma ',' = return ()
          comma  _  = pfail

toRange :: Assignment -> [Int]
toRange (Assignment x y) = [x..y]

hasOverlap :: Assignment -> Assignment -> Bool
hasOverlap a b = any (`elem` toRange b) $ toRange a

overlaps :: ElfPair -> Bool
overlaps (ElfPair a b) = hasOverlap a b || hasOverlap b a

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let pairs = map read $ lines cts

  print $ length $ filter id $ map overlaps pairs
