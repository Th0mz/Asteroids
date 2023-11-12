-- This module contains the data types which represent the state of the game in the context of the Asteroids
{-# LANGUAGE InstanceSigs #-}
module Asteroid where

import Graphics.Gloss (white, circle, color, Picture (Translate), Point, Vector, translate)
import qualified Graphics.Gloss as Data (Point, Vector, BitmapData)
import Model

import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations
import Auxiliary.Constants
import Hitbox

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderAsteroidHB :: Asteroid -> IO Picture
renderAsteroidHB asteroid = do
    let (x, y) = hPosition hitBox
    return $ Translate x y $ color white (circle radius)
  where
    hitBox = aHitBox asteroid
    radius = hRadius hitBox

renderAsteroid :: Asteroid -> IO Picture
renderAsteroid asteroid = do
    let (x, y) = hPosition hitBox
    skin <- aSkin asteroid
    return $ translate x y skin
    where
        hitBox = aHitBox asteroid

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

-- TODO : spawnAsteroid

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
moveAsteroid delta = map moveSingleAsteroid
  where
    moveSingleAsteroid asteroid = asteroid {aHitBox = newHitBox}
      where
        hitBox = aHitBox asteroid
        velocity = aVelocity asteroid
        newHitBox = moveHitBox delta velocity hitBox

-- exploding (creates 2 smaller asteroids when one is shot)
explodeAsteroid :: Asteroid -> [Asteroid]
explodeAsteroid asteroid
    | aCollided asteroid = case aSize asteroid of
        Large -> []--[randomMediumAsteroid, randomMediumAsteroid] 
        Medium -> []--[randomSmallAsteroid, randomSmallAsteroid]
        Small -> []
    | otherwise = [asteroid]
    where
        createExplodedAsteroid newSize = asteroid {
            aHitBox = (aHitBox asteroid) { hPosition = (0,0)},--aHitBox asteroid },
            aVelocity = (40, 40),
            aSize = newSize
        }
