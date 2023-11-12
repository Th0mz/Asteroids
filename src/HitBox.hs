module Hitbox where

import Graphics.Gloss
import Model
import Auxiliary.Operations

checkCollisions :: GameState -> GameState
checkCollisions gameState = undefined
    where 
        spaceship = gsSpaceship gameState
        bullets = gsBullets gameState
        asteroids = gsAsteroids gameState
        ufos = gsUfos gameState


-- auxiliary function
moveHitBox :: Float -> Vector -> HitBox -> HitBox
moveHitBox delta velocity hitBox = 
    hitBox {hPosition = newPosition}
    where 
        position = hPosition hitBox
        radius = hRadius hitBox
        newPosition = wrapAroundPos radius $ translatePos delta position velocity