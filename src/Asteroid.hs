-- This module contains the data types which represent the state of the game in the context of the Asteroids
module Asteroid where

import Graphics.Gloss (white, circle, color, Picture (Translate), Point, Vector, translate)
import qualified Graphics.Gloss as Data (Point, Vector, BitmapData)
import Model ( Asteroid (MkAsteroid, aSkin, aHitBox, aVelocity, aExploding, aSize),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsAsteroids)
               )

import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderAsteroidHB :: Asteroid -> IO Picture
renderAsteroidHB asteroid =
    return $
    Translate x y $
    color white (circle radius)
    where
        hitBox = aHitBox asteroid
        (x, y) = hPosition hitBox
        radius = hRadius hitBox

renderAsteroid :: Asteroid -> IO Picture
renderAsteroid asteroid = do
    skin <- aSkin asteroid
    return (translate x y skin)
    where
        hitBox = aHitBox asteroid
        (x, y) = hPosition hitBox

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

stepAsteroid :: Float -> GameState -> GameState
stepAsteroid delta gameState@(MkGameState {gsAsteroids = asteroids}) =
    gameState {gsAsteroids = newAsteroids}
    where
        newAsteroids = [newAsteroid asteroid | asteroid <- asteroids]
        newAsteroid asteroid = asteroid {aHitBox = newHitBox}
          where
            hitBox = aHitBox asteroid
            velocity = aVelocity asteroid
            newHitBox = moveHitBox delta velocity hitBox

moveAsteroid :: Float -> [Asteroid] -> [Asteroid]
moveAsteroid delta asteroids = map moveSingleAsteroid asteroids
  where
    moveSingleAsteroid asteroid = asteroid {aHitBox = newHitBox}
      where
        hitBox = aHitBox asteroid
        velocity = aVelocity asteroid
        newHitBox = moveHitBox delta velocity hitBox