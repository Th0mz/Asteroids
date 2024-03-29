-- This module defines how the state changes in response to time and user input
module Controller where

import Model
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.IO
import Spaceship
import Asteroid
import UFO
import Auxiliary.Operations
import Bullet (stepBullets)
import qualified Data.Set as S
import Hitbox (checkCollisions)
import HighScores
import Auxiliary.Constants
import Data.IntMap (keys)
import System.Exit (exitSuccess)


-- handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gameState@(MkGameState {gsScreen = screen, gsIsPaused = isPaused}) =
        case screen of
            Main       -> mainStep secs gameState
            Game       -> gameStep secs gameState
            Pause      -> pauseStep secs gameState
            HighScores -> highScoresStep secs gameState


-----------------------------------------
--              M A I N                --
-----------------------------------------
setupNewGame :: GameState -> GameState
setupNewGame gameState = setupSpaceShip $
                         gameState {
                            gsScreen = Game,
                            gsEnemyTimer = timeBetweenUFOs,
                            gsAsteroids = [],
                            gsUfos = [],
                            gsBullets = [],
                            gsScore = 0,
                            gsIsPaused = False,
                            gsHighScoreCheck = False
                        }

mainStep :: Float -> GameState -> IO GameState
mainStep _ gameState@(MkGameState {gsKeys = keys})
    | S.member KBenter keys = return $ setupNewGame gameState
    | otherwise = return gameState



-----------------------------------------
--              G A M E                --
-----------------------------------------

-- high-order functions

spawnAsteroids :: GameState -> IO GameState
spawnAsteroids gameState@(MkGameState {gsAsteroids = asteroids})
    -- add asteroids if asteroid list is empty
    | null asteroids = setupAsteroids 5 gameState
    | otherwise = return gameState

-- TODO : spawn enemies
spawnUFO :: Float -> GameState -> IO GameState
spawnUFO delta gameState@(MkGameState {gsUfos = ufos, gsEnemyTimer = timer})
    | timer < 0 = do
        gameState' <- addUFO gameState
        return $ gameState' {gsEnemyTimer = timeBetweenUFOs}
    | otherwise = return $ gameState {gsEnemyTimer = timer - delta}



gameStep :: Float -> GameState -> IO GameState
gameStep secs gameState = do
    gameState' <- spawnUFO secs
                $ checkGameOver
                $ checkPause
                $ checkCollisions
                $ stepBullets secs
                $ stepUfo secs
                $ stepAsteroid secs
                $ stepSpaceShip secs gameState 

    spawnAsteroids gameState'
    where
        -- high-order events
        checkPause :: GameState -> GameState
        checkPause gameState@(MkGameState {gsKeys = keys, gsIsPaused = isPaused})
            | S.notMember KBpause keys &&  isPaused = gameState{gsIsPaused = False}
            | S.member KBpause keys && not isPaused = gameState{gsScreen = Pause}
            | otherwise = gameState

        checkGameOver :: GameState -> GameState
        checkGameOver gameState
            | sLives (gsSpaceship gameState) <= 0 = gameState{gsScreen = HighScores}
            | otherwise = gameState

-----------------------------------------
--             P A U S E               --
-----------------------------------------
pauseStep :: Float -> GameState -> IO GameState
pauseStep _ gameState@(MkGameState {gsKeys = keys, gsIsPaused = isPaused})
        | S.notMember KBpause keys && not isPaused = return $ gameState {gsIsPaused = True}
        | S.member    KBpause keys &&     isPaused = return $ gameState {gsScreen = Game}
        | otherwise = return gameState


-----------------------------------------
--         H I G H S C O R E S         --
-----------------------------------------
-- closeWindow >> return ()
highScoresStep :: Float -> GameState -> IO GameState
highScoresStep _ gameState@(MkGameState {gsKeys = keys})  
    | not $ gsHighScoreCheck gameState = return $ gameState {gsHighScores = addHighScore playerName (gsScore gameState) (gsHighScores gameState), gsHighScoreCheck = True}
    | S.member KBenter keys = return $ setupNewGame gameState 
    | S.member KBquit  keys = do
                              saveHighScoresToFile highScoresPath $ gsHighScores gameState
                              exitSuccess
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
        Char 'q'            -> KBquit  -- q key
        _                   -> KBnone  -- key not recognized

-- key released
toKeyboardKey _ = KBnone