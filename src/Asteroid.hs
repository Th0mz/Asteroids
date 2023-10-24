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
import Auxiliary.Constants
import System.Random

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderAsteroidHB :: Asteroid -> IO Picture
renderAsteroidHB asteroid = do
    --(x, y) <- randomAsteroidPosition asteroid, doesn't yet do the right thing
    let (x, y) = hPosition hitBox
    return $ Translate x y $ color white (circle radius)
  where
    hitBox = aHitBox asteroid
    radius = hRadius hitBox

renderAsteroid :: Asteroid -> IO Picture
renderAsteroid asteroid = do
    --(x, y) <- randomAsteroidPosition asteroid, doesn't yet do the right thing
    let (x, y) = hPosition hitBox
    skin <- aSkin asteroid
    return $ translate x y skin
    where 
        hitBox = aHitBox asteroid

randomAsteroidPosition :: Asteroid -> IO (Float, Float)
randomAsteroidPosition asteroid = do
    randomX <- randomRIO (fromIntegral (- windowWidth `div` 2), fromIntegral (windowWidth `div` 2))
    randomY <- randomRIO (fromIntegral (- windowHeight `div` 2), fromIntegral (windowHeight `div` 2))
    return (randomX, randomY)

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