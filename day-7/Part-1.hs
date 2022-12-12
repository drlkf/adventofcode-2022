{-# LANGUAGE NumericUnderscores #-}
module Main where

import           Data.Char                       (isDigit)
import           Data.Map                        (Map, empty, insert)
import qualified Data.Map                        as M (foldl)
import           System.Environment              (getArgs)
import           Text.ParserCombinators.ReadP    (char, choice, eof, many1,
                                                  satisfy, string)
import           Text.ParserCombinators.ReadPrec (lift, readPrec_to_P)
import           Text.Read                       (readPrec)

data File = File Int String

data Command =
    Cd String
  | Ls

instance Read Command where
  readPrec = lift $ choice [readCd, readLs]
    where readCd = do
            _ <- string "cd "
            dir <- many1 $ satisfy (const True)
            eof
            return $ Cd dir
          readLs = do
            _ <- string "ls"
            eof
            return Ls

data Input =
    Cmd Command
  | FileI File
  | Dir String

data Path =
    Directory (Map String Path)
  | FileP Int

instance Read Input where
  readPrec = lift $ choice [parseCommand, parseFile, parseDirectory]
    where parseCommand = do
            _ <- string "$ "
            cmd <- readPrec_to_P readPrec 0
            eof
            return $ Cmd cmd
          parseFile = do
            sz <- read <$> many1 (satisfy isDigit)
            _ <- char ' '
            path <- many1 anyChar
            eof
            return $ FileI $ File sz path
          parseDirectory = do
            _ <- string "dir "
            path <- many1 anyChar
            eof
            return $ Dir path
          anyChar = satisfy (const True)

addToDir :: Path -> String -> Path -> Path
addToDir (Directory paths) name subpath = Directory $ insert name subpath paths
addToDir _ _ _ = error "trying to add subpath to wrong path"

interpret :: Path -> [Input] -> (Path, [Input])
interpret dir []                       = (dir, [])
interpret dir (Cmd (Cd "..") : cmds)   = (dir, cmds)
interpret dir (Cmd (Cd subdir) : cmds) =
  interpret dir' cmds'
  where (subpath, cmds') =
          interpret (Directory empty) cmds
        dir' = addToDir dir subdir subpath
interpret dir (Cmd _ : cmds)           = interpret dir cmds
interpret dir (Dir subdir : cmds) = interpret dir' cmds
  where dir' = addToDir dir subdir $ Directory empty
interpret dir (FileI (File sz name) : cmds) = interpret dir' cmds
  where dir' = addToDir dir name $ FileP sz

computeSize :: Path -> Int
computeSize (FileP sz)    = sz
computeSize (Directory dirs) = M.foldl acc 0 dirs
  where acc sz m = sz + computeSize m

directories :: Path -> [Path]
directories d@(Directory paths) = d : M.foldl acc [] paths
  where acc dirs m = dirs ++ directories m
directories _ = []

main :: IO ()
main = do
  args <- getArgs
  let readInput = if null args
                  then getContents
                  else readFile $ head args

  cts <- lines <$> readInput
  let inputs = map read cts :: [Input]
      root = Directory empty
      (dirs, _) = interpret root inputs
  print $ sum $ filter (< 100_000) $ map computeSize $ directories dirs
