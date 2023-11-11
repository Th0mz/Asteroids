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
import Graphics.Gloss.Interface.IO.Game (Event(EventKey))
import qualified Data.Set as S


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs = return
            . stepBullets secs
            . stepUfo secs
            . stepAsteroid secs
            . stepSpaceShip secs

-- | Handle user input
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
        Char 'p'            -> KBpause -- p key
        _                   -> KBnone  -- key not recognized

-- key released
toKeyboardKey _ = KBnone