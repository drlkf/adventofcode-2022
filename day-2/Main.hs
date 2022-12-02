module Main where

import           System.Environment              (getArgs)
import           Text.ParserCombinators.ReadPrec (get, pfail)
import           Text.Read                       (readPrec)

data Shape = Rock | Paper | Scissors
  deriving Eq

data Round = Round Shape Shape

instance Read Round where
  readPrec = do
    p1 <- get >>= p1Choices
    _ <- get >>= space
    p2 <- get >>= p2Choices
    return $ Round p1 p2
    where p1Choices 'A' = return Rock
          p1Choices 'B' = return Paper
          p1Choices 'C' = return Scissors
          p1Choices  _  = pfail

          p2Choices 'X' = return Rock
          p2Choices 'Y' = return Paper
          p2Choices 'Z' = return Scissors
          p2Choices  _  = pfail

          space ' ' = return ()
          space _   = pfail

data Outcome = Win | Draw | Lose

play :: Round -> Outcome
play (Round Rock Paper)     = Win
play (Round Rock Scissors)  = Lose
play (Round Paper Rock)     = Lose
play (Round Paper Scissors) = Win
play (Round Scissors Rock)  = Win
play (Round Scissors Paper) = Lose
play _                      = Draw

class Score a where
  score :: a -> Int

instance Score Shape where
  score Rock     = 1
  score Paper    = 2
  score Scissors = 3

instance Score Outcome where
  score Win  = 6
  score Lose = 0
  score Draw = 3

instance Score Round where
  score r@(Round _ p1) = score p1 + score (play r)

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let rounds = map read $ lines cts :: [Round]
  print $ sum $ map score rounds
