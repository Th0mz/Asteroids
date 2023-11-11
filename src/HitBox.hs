module Hitbox where

import Graphics.Gloss
import Model
import Auxiliary.Operations

checkCollisions :: GameState -> GameState
checkCollisions = undefined

-- auxiliary function
moveHitBox :: Float -> Vector -> HitBox -> HitBox
moveHitBox delta velocity hitBox = 
    hitBox {hPosition = newPosition}
    where 
        position = hPosition hitBox
        radius = hRadius hitBox
        newPosition = wrapAroundPos radius $ translatePos delta position velocity