module HighScores where

import Graphics.Gloss
import GHC.Generics
import Data.Aeson as A
import Data.ByteString.Lazy as BSL
import Data.List (sortBy)

type Score = Int
type Name = String
type HSEntry = (Name, Score)
type HighScores = [HSEntry]

loadHighScores :: String -> HighScores
loadHighScores _ = [] 

addHighScore :: Name -> Score -> HighScores -> HighScores
addHighScore name score highScores = (name, score) : highScores

loadHighScoresFromFile :: FilePath -> IO HighScores
loadHighScoresFromFile filePath = do
    scores <- A.decodeFileStrict filepath -- Aeson
    return $ case scores of
        Just s -> s
        Nothing -> []

saveHighScoresToFile :: FilePath -> HighScores -> IO()
saveHighScoresToFile filePath highScores = do
    BSL.writeFile filePath (A.encode highScores) --Aeson

displayTopHighScores :: HighScores -> IO ()
displayTopHighScores highScores = do
  let top10Scores = take 10 $ sortBy (flip compare `on` snd) highScores
  mapM_ (\(name, score) -> putStrLn $ name ++ ": " ++ show score) top10Scores