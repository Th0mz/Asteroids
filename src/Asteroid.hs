-- This module contains the data types which represent the state of the game in the context of the Asteroids
module Asteroid where

import qualified Graphics.Gloss as Data (Point, Vector, BitmapData)

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

-- very similar to the asteroid movement
-- stepSpaceShip :: Float -> GameState -> GameState
-- stepSpaceShip delta gameState@(MkGameState {gsSpaceship = spaceship}) =
--     gameState {gsSpaceship = newSpaceship}
--     where
--         hitBox = sHitBox spaceship
--         velocity = sVelocity spaceship
--         newSpaceship = spaceship {sHitBox = moveHitBox delta velocity hitBox}
