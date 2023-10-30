-- This module contains the data types which represent the state of the game in the context of the Spaceship
module Spaceship where

import Graphics.Gloss ( white, circle, color, Picture (Translate, Rotate), Point, Vector, translate )
import Model ( Spaceship (MkSpaceship, sHitBox, sVelocity, sSkin, sDirection),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsSpaceship, gsKeyboard), KeyBoard (KBup, KBleft, KBright)

             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Data.Vector (argV, rotateV)
import Auxiliary.Constants (spaceshipBitmap, spaceshipRotationSpeed)

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --
renderSpaceshipHB :: Spaceship -> IO Picture
renderSpaceshipHB spaceship = 
    return $
    Translate x y $ 
    color white (circle radius)
    where 
        hitBox = sHitBox spaceship
        (x, y) = hPosition hitBox
        radius = hRadius hitBox

renderSpaceship :: Spaceship -> IO Picture
renderSpaceship spaceship = do
    skin <- sSkin spaceship
    return (translate x y $ Rotate direction skin)
    where
        direction = radToDeg $ argV $ sDirection spaceship
        hitBox    = sHitBox spaceship
        (x, y)    = hPosition hitBox


-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --
stepSpaceShip :: Float -> GameState -> GameState
stepSpaceShip delta gameState@(MkGameState {gsSpaceship = spaceship, gsKeyboard = keyboard}) =
    gameState {gsSpaceship = moveSpaceShip delta  
                           $ rotateSpaceShip delta keyboard spaceship
              }

moveSpaceShip :: Float -> Spaceship -> Spaceship
moveSpaceShip delta spaceship = 
    spaceship {sHitBox = moveHitBox delta velocity hitBox}
    where 
        hitBox = sHitBox spaceship
        velocity = sVelocity spaceship

rotateSpaceShip :: Float -> KeyBoard -> Spaceship -> Spaceship
rotateSpaceShip delta keyboard spaceship = 
    case keyboard of 
        KBleft  -> spaceship {sDirection = rotateV (- rotationDelta) currDirection} 
        KBright -> spaceship {sDirection = rotateV    rotationDelta  currDirection}
        _       -> spaceship
    where 
        currDirection = sDirection spaceship
        rotationDelta = spaceshipRotationSpeed * delta

shootBulletFromSpaceship :: GameState -> GameState
shootBulletFromSpaceship = undefined