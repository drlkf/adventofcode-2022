module Main where

import           Data.Char                       (isAlpha, isDigit)
import           Data.List
import           Data.Map                        (Map, adjust, elems, fromList,
                                                  (!))
import           Data.Maybe                      (mapMaybe)
import           System.Environment              (getArgs)
import           Text.ParserCombinators.ReadP    (between, char, eof, many,
                                                  many1, satisfy, string, (<++))
import           Text.ParserCombinators.ReadPrec (lift, readPrec_to_P)
import           Text.Read                       (readPrec)

newtype Crate = Crate
  { uncrate :: Char
  }

data Move = Move
  { quantity :: Int
  , source   :: Int
  , target   :: Int
  }

instance Read Move where
  readPrec = do
    _ <- lift $ string "move "
    q <- read <$> number
    _ <- lift $ string " from "
    s <- read <$> number
    _ <- lift $ string " to "
    t <- read <$> number
    return $ Move q s t
    where number = lift $ many1 $ satisfy isDigit

-- parser transient types
data CrateLineElement = NoCrate | CrateE Crate

instance Read CrateLineElement where
  readPrec = lift $ noCrate <++ parseCrate
    where noCrate = string "   " >> return NoCrate
          parseCrate = CrateE . Crate <$> between (char '[') (char ']') (satisfy isAlpha)

newtype CrateLine = CrateLine
  { unline :: [CrateLineElement]
  }

instance Read CrateLine where
  readPrec = do
    first <- readPrec
    elements <- lift $ many $ readPrec_to_P readCrate 0
    lift eof
    return $ CrateLine $ first : elements
    where readCrate = do
            _ <- lift $ char ' '
            readPrec

makeStacks :: [Int] -> [CrateLine] -> Map Int [Crate]
makeStacks numbers crateLines = fromList $ zip numbers crates
  where crates = map catCrates $ transpose $ map unline crateLines
        catCrates = mapMaybe extract
        extract NoCrate    = Nothing
        extract (CrateE c) = Just c

move :: Move -> Map Int [Crate] -> Map Int [Crate]
move m stacks =
  adjust (drop (length pile)) from $ adjust (pile ++) to stacks
  where qty = quantity m
        from = source m
        to = target m
        -- for part 2, remove the 'reverse' call
        pile = reverse $ take qty $ stacks ! from

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- lines <$> readInput
  let (part1, part2) = break null cts
      stackInput = init part1
      stacksLine = last part1
      moveInput = dropWhile null part2

      stackLines = map read stackInput
      stackNumbers = map read $ words stacksLine
      stacks = makeStacks stackNumbers stackLines
      moves = map read moveInput :: [Move]
      stacks' = foldl (flip move) stacks moves
      safeHead []    = Nothing
      safeHead (x:_) = Just x
      topCrates = map uncrate $ mapMaybe safeHead $ elems stacks'

  putStrLn topCrates
