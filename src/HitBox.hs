module Hitbox where

import Graphics.Gloss
import Model
import Auxiliary.Operations

checkCollisions :: GameState -> GameState
checkCollisions gameState = foldr isSpaceshipColliding gameState [spaceship]
    where
        spaceship = gsSpaceship gameState
        -- spaceship can collide with: ufos, asteroids and bullets
        isSpaceshipColliding :: Spaceship -> GameState -> GameState
        isSpaceshipColliding spaceship gameState = checkUFOsCollision spaceship $
                                                   checkBulletsCollision spaceship $ 
                                                   checkAsteroidsCollision spaceship gameState
        
        asteroids = gsAsteroids gameState
        -- asteroid can collide with: ufos, bullets and the spaceship
        isAsteroidColliding :: Asteroid -> GameState -> GameState
        isAsteroidColliding asteroid gameState = checkUFOsCollision asteroid $
                                                 checkBulletsCollision asteroid $
                                                 checkSpaceshipCollision asteroid gameState
        
        ufos = gsUfos gameState                     
        -- ufo can collide with: bullets, asteroids and the spaceship
        isUFOColliding :: UFO -> GameState -> GameState
        isUFOColliding ufo gameState = checkBulletsCollision ufo $
                                       checkAsteroidsCollision ufo $
                                       checkSpaceshipCollision ufo gameState
        
        bullets = gsBullets gameState
        -- bullets can collide with: ufos, asteroids and the spaceship
        isBulletColliding :: Bullet -> GameState -> GameState
        isBulletColliding bullet gameState = checkUFOsCollision bullet $
                                             checkAsteroidsCollision bullet $
                                             checkSpaceshipCollision bullet gameState


checkSpaceshipCollision :: Collidable a => a -> GameState -> GameState
checkSpaceshipCollision obj gameState@(MkGameState {gsSpaceship = spaceship})
    -- if the object already collided with something just skip
    | didCollide obj = gameState
    -- check if the object collided with the spaceship
    | isColliding obj spaceship = afterCollision obj gameState
    | otherwise = gameState 

checkAsteroidsCollision :: Collidable a => a -> GameState -> GameState
checkAsteroidsCollision obj gameState@(MkGameState {gsAsteroids = asteroids})
    -- if the object already collided with something just skip
    | didCollide obj = gameState
    -- check if the object collided with some asteroid
    | any (isColliding obj) asteroids = afterCollision obj gameState
    | otherwise = gameState 


checkBulletsCollision :: Collidable a => a -> GameState -> GameState
checkBulletsCollision obj gameState@(MkGameState {gsBullets = bullets})
    -- if the object already collided with something just skip
    | didCollide obj = gameState
    -- check if the object collided with some bullet
    | any (isColliding obj) bullets = afterCollision obj gameState
    | otherwise = gameState 
    
checkUFOsCollision :: Collidable a => a -> GameState -> GameState
checkUFOsCollision obj gameState@(MkGameState {gsUfos = ufos}) 
    -- if the object already collided with something just skip
    | didCollide obj = gameState
    -- check if the object collided with some ufo
    | any (isColliding obj) ufos = afterCollision obj gameState
    | otherwise = gameState 

-- auxiliary function
moveHitBox :: Float -> Vector -> HitBox -> HitBox
moveHitBox delta velocity hitBox = 
    hitBox {hPosition = newPosition}
    where 
        position = hPosition hitBox
        radius = hRadius hitBox
        newPosition = wrapAroundPos radius $ translatePos delta position velocity