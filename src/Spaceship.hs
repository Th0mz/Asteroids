-- This module contains the data types which represent the state of the game in the context of the Spaceship
module Spaceship where

import Graphics.Gloss ( white, circle, color, Picture (Translate), Point, Vector )
import Model ( Spaceship (MkSpaceship, sHitBox, sVelocity),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsSpaceship)

             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --
renderSpaceship :: Spaceship -> Picture
renderSpaceship spaceship = 
    Translate x y $ 
    color white (circle radius)
    where 
        hitBox = sHitBox spaceship
        (x, y) = hPosition hitBox
        radius = hRadius hitBox

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --
stepSpaceShip :: Float -> GameState -> GameState
stepSpaceShip delta gameState@(MkGameState {gsSpaceship = spaceship}) =
    gameState {gsSpaceship = newSpaceship}
    where
        hitBox = sHitBox spaceship
        velocity = sVelocity spaceship
        newSpaceship = spaceship {sHitBox = moveHitBox delta velocity hitBox}