-- This module contains the data types which represent the state of the game in the context of the Spaceship
module Spaceship where

import Graphics.Gloss ( white, circle, color, Picture (Translate, Rotate, Color), Point, Vector, translate, red, line )
import Model ( Spaceship (..),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsSpaceship, gsKeys, gsSpaceshipSkin), Keys, KeyBoard (KBup, KBleft, KBright, KBspace), Cooldown, Collidable (..), getIdentifier
             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Data.Vector (argV, rotateV, mulSV, normalizeV)
import Auxiliary.Constants (spaceshipBitmap, spaceshipRotationSpeed, spaceshipMaxSpeed, spaceshipSize, shootingCoolDown, spaceshipAcceleration, spaceshipFriction)
import Bullet (spawnBullet)
import Hitbox

import qualified Data.Set as S

-----------------------------------------
--     I N I T I A L I Z A T I O N     --
-----------------------------------------
setupSpaceShip :: GameState -> GameState
setupSpaceShip gameState@(MkGameState {gsSpaceshipSkin = skin}) = gameState' {
        gsSpaceship = MkSpaceship {
            sId = id,
            sSkin = skin,
            sLives = 3,
            sCooldown = 0,
            sHitBox = MkHitBox {hPosition = (0, 0), hRadius = spaceshipSize / 2},
            sDirection = (0, 1),
            sVelocity = (0, 0),
            sExploding = False,
            sCollided = False
        }
    }
    where 
        (id, gameState') = getIdentifier gameState

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --
renderSpaceshipHB :: Spaceship -> IO Picture
renderSpaceshipHB spaceship = 
    return $
    Translate x y $ 
    color white (circle radius)
    where 
        hitBox = sHitBox spaceship
        (x, y) = hPosition hitBox
        radius = hRadius hitBox

renderSpaceshipDir :: Spaceship -> IO Picture
renderSpaceshipDir spaceship = 
    return 
    $ Color red 
    $ line [(x, y), (x + dirX, y + dirY)]
    where 
        hitBox = sHitBox spaceship
        (x, y) = hPosition hitBox
        (dirX, dirY) = mulSV 100 (sDirection spaceship)


renderSpaceship :: Spaceship -> IO Picture
renderSpaceship spaceship = do
    skin <- sSkin spaceship
    return (translate x y $ Rotate direction skin)
    where
        direction = radToDeg $ - (argV $ sDirection spaceship)
        hitBox    = sHitBox spaceship
        (x, y)    = hPosition hitBox


-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --
stepSpaceShip :: Float -> GameState -> GameState
stepSpaceShip delta gameState@(MkGameState {gsSpaceship = spaceship, gsKeys = keys}) = 
    -- shoot bullet from updated gameState if the space key is pressed 
    shootBulletFromSpaceship keys $
    -- update game state
    gameState {gsSpaceship = moveSpaceShip delta
                           $ updateCooldown delta
                           $ updateVelocity delta keys  
                           $ rotateSpaceShip delta keys spaceship
              }

moveSpaceShip :: Float -> Spaceship -> Spaceship
moveSpaceShip delta spaceship = 
    spaceship {sHitBox = moveHitBox delta velocity hitBox}
    where 
        hitBox = sHitBox spaceship
        velocity = sVelocity spaceship

updateVelocity :: Float -> Keys -> Spaceship -> Spaceship
updateVelocity delta keys spaceship
    -- apply acceleration thrust to velocity
    | S.member KBup keys = spaceship {sVelocity = addVectors (sVelocity spaceship) acceleration}
    -- apply friction
    | otherwise          = spaceship {sVelocity = scaleVector (spaceshipFriction * delta) (sVelocity spaceship)}
    where 
        direction = sDirection spaceship
        acceleration = mulSV (spaceshipAcceleration * delta) (normalizeV direction) 

updateCooldown :: Float -> Spaceship -> Spaceship
updateCooldown delta spaceship = spaceship {sCooldown = sCooldown spaceship - delta}


rotateSpaceShip :: Float -> Keys -> Spaceship -> Spaceship
rotateSpaceShip delta keys spaceship  
    | S.member KBleft  keys = spaceship {sDirection = rotateV    rotationDelta  currDirection}
    | S.member KBright keys = spaceship {sDirection = rotateV (- rotationDelta) currDirection}
    | otherwise             = spaceship
    where 
        currDirection = sDirection spaceship
        rotationDelta = spaceshipRotationSpeed * delta



shootBulletFromSpaceship :: Keys -> GameState -> GameState
shootBulletFromSpaceship keys gameState@(MkGameState {gsSpaceship = spaceship}) 
    | S.member KBspace keys && canShoot spaceship 
                = addShootingCooldown $ spawnBullet position direction gameState
    | otherwise = gameState
    where
        sPosition = hPosition $ sHitBox spaceship
        direction =  sDirection spaceship
        position = translatePoint sPosition (mulSV ((spaceshipSize / 2) + 5) direction)
        
        canShoot :: Spaceship -> Bool
        canShoot spaceship = sCooldown spaceship <= 0

        addShootingCooldown :: GameState -> GameState
        addShootingCooldown gameState@(MkGameState {gsSpaceship = spaceship}) = 
            gameState {gsSpaceship = spaceship {sCooldown = shootingCoolDown}}