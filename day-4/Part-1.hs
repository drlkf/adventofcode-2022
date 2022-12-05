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
    a <- lift $ many $ satisfy isDigit
    get >>= dash
    b <- lift $ many $ satisfy isDigit
    return $ Assignment (read a) (read b)
      where dash '-' = return ()
            dash  _  = pfail

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

contains :: Assignment -> Assignment -> Bool
contains (Assignment ax ay) (Assignment bx by) = bx <= ax && by >= ay

overlaps :: ElfPair -> Bool
overlaps (ElfPair a b) = contains a b || contains b a

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let pairs = map read $ lines cts

  print $ length $ filter id $ map overlaps pairs
