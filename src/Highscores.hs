-- This module contains the functions which represent the highscores shown at the end of a game
module HighScores where

import Graphics.Gloss
import Data.List (sortBy)
import System.IO

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

import Data.Ord (comparing)
import Auxiliary.Constants

type Score = Int
type Name = String
type HSEntry = (Name, Score)
type HighScores = [HSEntry]

parseLine :: String -> Maybe HSEntry
parseLine line = case words line of
  [name, scoreStr] -> do
    score <- readMaybe scoreStr
    return (name, score)
  _ -> Nothing

loadHighScores :: FilePath -> IO HighScores
loadHighScores filePath = do
  content <- readFile filePath
  let entries = mapMaybe parseLine (lines content)
  return entries

sortByDescendingSnd :: [(Name, Score)] -> [HSEntry]
sortByDescendingSnd = sortBy (flip $ comparing snd)

addHighScore :: Name -> Score -> HighScores -> HighScores
addHighScore name score highScores = take highScoreSize  $ sortByDescendingSnd ((name, score) : highScores)  

saveHighScoresToFile :: FilePath -> HighScores -> IO ()
saveHighScoresToFile filePath highScores = do
    let entries = unlines $ map showEntry highScores
    writeFile filePath entries
    where
        showEntry :: HSEntry -> String
        showEntry (name, score) = name ++ " " ++ show score