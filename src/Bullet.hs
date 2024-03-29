module Bullet where

import Model
import Graphics.Gloss
import Auxiliary.Constants
import Graphics.Gloss.Data.Vector
import Auxiliary.Operations
import Hitbox

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderBullet :: Bullet -> IO Picture
renderBullet bullet = 
    return $ 
    Translate x y $ 
    color white (circle radius)
    where 
        hitBox = bHitBox bullet
        (x, y) = hPosition hitBox
        radius = hRadius hitBox

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

spawnBullet :: Point -> Vector -> GameState -> GameState
spawnBullet position direction gameState =
        gameState'{gsBullets = bullet : gsBullets gameState}
    where
        (id, gameState') = getIdentifier gameState
        velocity = mulSV bulletSpeed (normalizeV direction)
        bullet = MkBullet {
            bId = id,
            bHitBox = MkHitBox { hPosition = position, hRadius = bulletRadius },
            bVelocity = velocity,
            bLifeTime = bulletLifetime,
            bCollided = False
        }

-- bullet garbage collector must be done here
stepBullets :: Float -> GameState -> GameState
stepBullets delta gameState@(MkGameState {gsBullets = bullets}) =
    gameState {gsBullets = filter isBulletAlive 
                 (map (updateBulletLifeTime delta 
                     . moveBullet delta) 
                  bullets)
               }

moveBullet :: Float -> Bullet -> Bullet
moveBullet delta bullet = 
    bullet {bHitBox = moveHitBox delta velocity hitBox}
    where 
        hitBox = bHitBox bullet
        velocity = bVelocity bullet

updateBulletLifeTime :: Float -> Bullet -> Bullet
updateBulletLifeTime delta bullet =
    bullet {bLifeTime = bLifeTime bullet - delta}

isBulletAlive :: Bullet -> Bool
isBulletAlive bullet = bLifeTime bullet > 0

