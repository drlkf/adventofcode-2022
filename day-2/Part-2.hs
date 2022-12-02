module Main where

import           System.Environment              (getArgs)
import           Text.ParserCombinators.ReadPrec (get, pfail)
import           Text.Read                       (readPrec)

data Shape = Rock | Paper | Scissors
  deriving Eq

data Outcome = Win | Draw | Lose

data Round = Round Shape Outcome

instance Read Round where
  readPrec = do
    p1 <- get >>= p1Choices
    _ <- get >>= space
    oc <- get >>= outcome
    return $ Round p1 oc
    where p1Choices 'A' = return Rock
          p1Choices 'B' = return Paper
          p1Choices 'C' = return Scissors
          p1Choices  _  = pfail

          outcome 'X' = return Lose
          outcome 'Y' = return Draw
          outcome 'Z' = return Win
          outcome  _  = pfail

          space ' ' = return ()
          space _   = pfail

play :: Round -> Shape
play (Round Rock Win)      = Paper
play (Round Rock Lose)     = Scissors
play (Round Paper Win)     = Scissors
play (Round Paper Lose)    = Rock
play (Round Scissors Win)  = Rock
play (Round Scissors Lose) = Paper
play (Round p2 Draw)       = p2

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
  score r@(Round _ outcome) = score outcome + score (play r)

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- readInput
  let rounds = map read $ lines cts :: [Round]
  print $ sum $ map score rounds
