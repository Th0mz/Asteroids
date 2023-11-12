-- This module contains the data types which represent the state of the game in the context of the UFO
module UFO where

import Graphics.Gloss ( white, circle, color, Picture (Translate), Point, Vector, translate )
import Model ( UFO (MkUfo, uHitBox, uVelocity, uSkin, uCollided),
               Spaceship (MkSpaceship, sHitBox, sVelocity, sSkin),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsUfos), Collidable (..)

             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations
import Spaceship
import Hitbox


-----------------------------------------
--     I N I T I A L I Z A T I O N     --
-----------------------------------------

-- initUfo :: IO UFO
-- initUfo = do
--     randomX <- randomRIO (windowMinX, windowMaxX)
--     randomY <- randomRIO (windowMinY, windowMaxY)
--     return MkUfo {
--         uSkin = Data.loadBMP ufoBitmap,
--         uHitBox = MkHitBox { hPosition = (randomX, randomY), hRadius = ufoSize / 2 },
--         uVelocity = (30, 30),
--         uCollided = False
--     }
-- 
-- addUfo :: UFO -> [UFO] -> [UFO]
-- addUfo ufo ufos = ufo : ufos

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderUfoHB :: UFO -> IO Picture
renderUfoHB ufo = do
    let (x, y) = hPosition hitBox
    return $ Translate x y $ color white (circle radius)
  where
    hitBox = uHitBox ufo
    radius = hRadius hitBox

renderUfo :: UFO -> IO Picture
renderUfo ufo = do
    let (x, y) = hPosition hitBox
    skin <- uSkin ufo
    return $ translate x y skin
    where 
        hitBox = uHitBox ufo

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

-- TODO : spawnUFO

stepUfo :: Float -> GameState -> GameState
stepUfo delta gameState@(MkGameState {gsUfos = ufos}) =
    gameState {gsUfos = newUfos}
    where
        newUfos = [newUfo ufo | ufo <- ufos]
        newUfo ufo = ufo {uHitBox = newHitBox}
          where
            hitBox = uHitBox ufo
            velocity = uVelocity ufo
            newHitBox = moveHitBox delta velocity hitBox

moveUfo :: Float -> [UFO] -> [UFO]
moveUfo delta = map moveSingleUfo
  where
    moveSingleUfo ufo = ufo {uHitBox = newHitBox}
      where
        hitBox = uHitBox ufo
        velocity = uVelocity ufo
        newHitBox = moveHitBox delta velocity hitBox

calcUfoSpaceshipPos :: Spaceship -> UFO -> Float -> Point
calcUfoSpaceshipPos spaceship ufo delta = 
  translatePos delta (hPosition (sHitBox spaceship)) (uVelocity ufo)

shootBulletFromUfo :: GameState -> GameState
shootBulletFromUfo = undefined