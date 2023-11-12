-- This module defines how the state changes in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Spaceship
import Asteroid
import UFO
import Auxiliary.Operations
import Bullet (stepBullets)
import qualified Data.Set as S


-- handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs  gameState@(MkGameState {gsScreen = screen}) =
    case screen of 
        Main       -> mainStep secs gameState
        Game       -> gameStep secs gameState
        Pause      -> pauseStep secs gameState
        HighScores -> highScoresStep secs gameState


mainStep :: Float -> GameState -> IO GameState
mainStep _ gameState@(MkGameState {gsKeys = keys})
    | S.member KBenter keys = return $ gameState {gsScreen = Game}
    | otherwise = return gameState

gameStep :: Float -> GameState -> IO GameState
gameStep secs = return
              . checkPause
              . stepBullets secs
              . stepUfo secs
              . stepAsteroid secs
              . stepSpaceShip secs
    where
        checkPause :: GameState -> GameState
        checkPause gameState@(MkGameState {gsKeys = keys, gsIsPaused = isPaused})
            | S.notMember KBpause keys &&  isPaused = gameState{gsIsPaused = False}
            | S.member KBpause keys && not isPaused = gameState{gsScreen = Pause}
            | otherwise = gameState

pauseStep :: Float -> GameState -> IO GameState
pauseStep _ gameState@(MkGameState {gsKeys = keys, gsIsPaused = isPaused})
        | S.notMember KBpause keys && not isPaused = return $ gameState {gsIsPaused = True} 
        | S.member    KBpause keys &&     isPaused = return $ gameState {gsScreen = Game}
        | otherwise = return gameState

highScoresStep :: Float -> GameState -> IO GameState
highScoresStep _ gameState@(MkGameState {gsKeys = keys})
    | S.member KBScore keys && gsScreen gameState /= HighScores = return $ gameState {gsScreen = HighScores}
    | S.member KBScore keys && gsScreen gameState == HighScores = return $ gameState {gsScreen = Game}
    | otherwise = return gameState

-- handle user input
input :: Event -> GameState -> IO GameState
input event@(EventKey _ state _ _) gameState =
    case toKeyboardKey event of
        KBnone -> return gameState
        key    -> case state of
            Down -> return $ gameState {gsKeys = S.insert key (gsKeys gameState)}
            Up   -> return $ gameState {gsKeys = S.delete key (gsKeys gameState)}

input _ gameState = return gameState



toKeyboardKey :: Event -> KeyBoard
toKeyboardKey (EventKey k _ _ _) =
    case k of
        SpecialKey KeyUp    -> KBup    -- up key
        SpecialKey KeyLeft  -> KBleft  -- left key
        SpecialKey KeyRight -> KBright -- right key
        SpecialKey KeySpace -> KBspace -- space bar
        SpecialKey KeyEnter -> KBenter -- enter key
        Char 'p'            -> KBpause -- p key
        Char 's'            -> KBScore -- s key
        _                   -> KBnone  -- key not recognized

-- key released
toKeyboardKey _ = KBnone