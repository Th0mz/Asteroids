-- This module contains the data types which represent the state of the game in the context of the Spaceship
module Spaceship where

import Graphics.Gloss ( white, circle, color, Picture (Translate, Rotate, Color), Point, Vector, translate, red, line )
import Model ( Spaceship (MkSpaceship, sHitBox, sVelocity, sSkin, sDirection),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsSpaceship, gsKeyboard), KeyBoard (KBup, KBleft, KBright)

             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Data.Vector (argV, rotateV, mulSV)
import Auxiliary.Constants (spaceshipBitmap, spaceshipRotationSpeed, spaceshipMaxSpeed)

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

renderSpaceshipDir :: Spaceship -> IO Picture
renderSpaceshipDir spaceship = 
    return 
    $ Color red 
    $ line [(x, y), (x + dirX, y + dirY)]
    where 
        hitBox = sHitBox spaceship
        (x, y) = hPosition hitBox
        (dirX, dirY) = mulSV 100 (sDirection spaceship)


renderSpaceship :: Spaceship -> IO Picture
renderSpaceship spaceship = do
    skin <- sSkin spaceship
    return (translate x y $ Rotate direction skin)
    where
        direction = radToDeg $ - (argV $ sDirection spaceship)
        hitBox    = sHitBox spaceship
        (x, y)    = hPosition hitBox


-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --
stepSpaceShip :: Float -> GameState -> GameState
stepSpaceShip delta gameState@(MkGameState {gsSpaceship = spaceship, gsKeyboard = keyboard}) =
    gameState {gsSpaceship = moveSpaceShip delta
                           $ updateVelocity delta keyboard  
                           $ rotateSpaceShip delta keyboard spaceship
              }

moveSpaceShip :: Float -> Spaceship -> Spaceship
moveSpaceShip delta spaceship = 
    spaceship {sHitBox = moveHitBox delta velocity hitBox}
    where 
        hitBox = sHitBox spaceship
        velocity = sVelocity spaceship

-- TODO : use acceleration to give a notion of space physics
updateVelocity :: Float -> KeyBoard -> Spaceship -> Spaceship
updateVelocity delta keyboard spaceship =
    case keyboard of
        KBup -> spaceship {sVelocity = mulSV spaceshipMaxSpeed direction}
        _    -> spaceship {sVelocity = (0, 0)}
    where 
        direction = sDirection spaceship


rotateSpaceShip :: Float -> KeyBoard -> Spaceship -> Spaceship
rotateSpaceShip delta keyboard spaceship = 
    case keyboard of 
        KBleft  -> spaceship {sDirection = rotateV    rotationDelta  currDirection}
        KBright -> spaceship {sDirection = rotateV (- rotationDelta) currDirection} 
        _       -> spaceship
    where 
        currDirection = sDirection spaceship
        rotationDelta = spaceshipRotationSpeed * delta

shootBulletFromSpaceship :: GameState -> GameState
shootBulletFromSpaceship = undefined