module HighScores where

import Graphics.Gloss
import Data.List (sortBy)
import System.IO

type Score = Int
type Name = String
type HSEntry = (Name, Score)
type HighScores = [HSEntry]

loadHighScores :: FilePath -> IO HighScores
loadHighScores filePath = do
    contents <- readFile filePath
    let entries = map readEntry $ lines contents
    return entries

addHighScore :: Name -> Score -> HighScores -> HighScores
addHighScore name score highScores = (name, score) : highScores

saveHighScoresToFile :: FilePath -> HighScores -> IO ()
saveHighScoresToFile filePath highScores = do
    let entries = unlines $ map showEntry highScores
    writeFile filePath entries

displayTopHighScores :: HighScores -> IO ()
displayTopHighScores highScores = do
    let top10Scores = take 10 $ sortByDescendingSnd highScores
    mapM_ (\(name, score) -> putStrLn $ name ++ ": " ++ show score) top10Scores

sortByDescendingSnd :: HighScores -> HighScores
sortByDescendingSnd = sortBy (\(_, score1) (_, score2) -> compare score2 score1)

readEntry :: String -> HSEntry
readEntry str = case words str of
    [name, score] -> (name, read score)
    _             -> error "Invalid entry format"

showEntry :: HSEntry -> String
showEntry (name, score) = name ++ " " ++ show score