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
    where
        readEntry :: String -> HSEntry
        readEntry str = case words str of
            [name, score] -> (name, read score)
            _             -> error "Invalid entry format"

addHighScore :: Name -> Score -> HighScores -> HighScores
addHighScore name score highScores = (name, score) : highScores

saveHighScoresToFile :: FilePath -> HighScores -> IO ()
saveHighScoresToFile filePath highScores = do
    let entries = unlines $ map showEntry highScores
    writeFile filePath entries
    where
        showEntry :: HSEntry -> String
        showEntry (name, score) = name ++ " " ++ show score

displayTopHighScores :: HighScores -> IO ()
displayTopHighScores highScores = do
    let topScores = take 10 $ sortByDescendingSnd highScores --change to all scores
    mapM_ (\(name, score) -> putStrLn $ name ++ ": " ++ show score) topScores
    where
        sortByDescendingSnd :: HighScores -> HighScores
        sortByDescendingSnd = sortBy (\(_, score1) (_, score2) -> compare score2 score1)