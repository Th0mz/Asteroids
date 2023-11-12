module Hitbox where

import Graphics.Gloss
import Model
import Auxiliary.Operations

checkCollisions :: GameState -> GameState
checkCollisions gameState = foldl checkAsteroidsCollision gameState [spaceship]
    where
        spaceship = gsSpaceship gameState


checkAsteroidsCollision :: Collidable a => GameState -> a -> GameState
checkAsteroidsCollision gameState@(MkGameState {gsAsteroids = asteroids}) obj
    | any (isColliding obj) asteroids = afterCollision obj gameState
    | otherwise = gameState 


-- checkBulletsCollision :: Collidable a => GameState -> a -> GameState
-- checkAsteroidsCollision gameState@(MkGameState {gsBullets = bullets}) obj
--     | any (isColliding obj) (filter (/= obj) bullets) = afterCollision obj gameState
--     | otherwise = gameState
-- 
-- checkUFOsCollision :: Collidable a => GameState -> a -> GameState
-- checkAsteroidsCollision gameState@(MkGameState {gsUfos = ufos}) obj
--     | any (isColliding obj) (filter (/= obj) ufos) = afterCollision obj gameState
--     | otherwise = gameState


-- auxiliary function
moveHitBox :: Float -> Vector -> HitBox -> HitBox
moveHitBox delta velocity hitBox = 
    hitBox {hPosition = newPosition}
    where 
        position = hPosition hitBox
        radius = hRadius hitBox
        newPosition = wrapAroundPos radius $ translatePos delta position velocity