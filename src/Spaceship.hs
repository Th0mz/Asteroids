-- This module contains the data types which represent the state of the game in the context of the Spaceship
module Spaceship where

import Graphics.Gloss ( white, circle, color, Picture (Translate), Point, Vector, translate )
import Model ( Spaceship (MkSpaceship, sHitBox, sVelocity, sSkin),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsSpaceship)

             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations

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
    return (translate x y skin)
    where
        hitBox = sHitBox spaceship
        (x, y) = hPosition hitBox

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --
stepSpaceShip :: Float -> GameState -> GameState
stepSpaceShip delta gameState@(MkGameState {gsSpaceship = spaceship}) =
    gameState {gsSpaceship = moveSpaceShip delta spaceship}

moveSpaceShip :: Float -> Spaceship -> Spaceship
moveSpaceShip delta spaceship = 
    spaceship {sHitBox = moveHitBox delta velocity hitBox}
    where 
        hitBox = sHitBox spaceship
        velocity = sVelocity spaceship