-- This module contains the data types which represent the state of the game in the context of the UFO
module UFO where

import Graphics.Gloss ( white, circle, color, Picture (Translate, Color), Point, Vector, translate, red, line )
import Model ( UFO (MkUfo, uHitBox, uVelocity, uSkin, uCollided, uId, uExploding, uShootTime),
               Spaceship (MkSpaceship, sHitBox, sVelocity, sSkin),
               HitBox (MkHitBox, hPosition, hRadius),
               GameState (MkGameState, gsUfos, gsUFOSkin, gsScore, gsSpaceship), Collidable (..), getIdentifier, updateElement

             )
import GHC.Num.BigNat (raiseDivZero_BigNat)
import Auxiliary.Operations
import Spaceship
import Hitbox
import System.Random
import Auxiliary.Constants
import Graphics.Gloss.Data.Vector
import Bullet (spawnBullet)


-----------------------------------------
--     I N I T I A L I Z A T I O N     --
-----------------------------------------

-- data UFO = MkUfo {
--     uId   :: Identifier,
--     uSkin :: IO Picture,
--     uHitBox :: HitBox,
--     uVelocity :: Data.Vector,
--     uExploding :: Exploding,
--     uCollided :: Collided
-- }

addUFO :: GameState -> IO GameState
addUFO gameState@(MkGameState { gsUFOSkin = skin }) = do
     randomX  <- randomRIO (windowMinX, windowMaxX)
     randomY  <- randomRIO (windowMinY, windowMaxY)
     randDirX <- randomRIO (-1.0 :: Float, 1.0)
     randDirY <- randomRIO (-1.0 :: Float, 1.0)

     return $ gameState' {
        gsUfos = MkUfo {
          uId = id,
          uSkin = skin,
          uHitBox = MkHitBox {hPosition = (randomX, randomY), hRadius = ufoSize / 2},
          uVelocity = mulSV ufoSpeed (normalizeV (randDirX, randDirY)),
          uExploding = False,
          uCollided = False,
          uShootTime = ufoShootingTimeout
        } : gsUfos gameState
     }
     where
      (id, gameState') = getIdentifier gameState


-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderUfoHB :: UFO -> IO Picture
renderUfoHB ufo = do
    let (x, y) = hPosition hitBox
    return $ Translate x y $ color white (circle radius)
  where
    hitBox = uHitBox ufo
    radius = hRadius hitBox

renderUfoDir :: Spaceship -> UFO -> IO Picture
renderUfoDir spaceship ufo = 
    return 
    $ Color red 
    $ line [(x, y), (x + dirX, y + dirY)]
    where 
        hitBox = uHitBox ufo
        (x, y) = hPosition hitBox
        (dirX, dirY) = mulSV 100 (normalizeV $ vectorFromPoints (hPosition $ sHitBox spaceship) (x, y))

renderUfo :: UFO -> IO Picture
renderUfo ufo = do
    let (x, y) = hPosition hitBox
    skin <- uSkin ufo
    return $ translate x y skin
    where
        hitBox = uHitBox ufo

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

-- TODO : spawnUFO

stepUfo :: Float -> GameState -> GameState
stepUfo delta gameState@(MkGameState {gsUfos = ufos}) =
    -- foldr isSpaceshipColliding gameState  [spaceship]
    foldr (shootBulletFromUfo delta) gameState' (gsUfos gameState')
    where
        gameState' = gameState {gsUfos = filter (not . uExploding) newUfos}
        newUfos = [newUfo ufo | ufo <- ufos]
        newUfo ufo = ufo {uHitBox = newHitBox}
          where
            hitBox = uHitBox ufo
            velocity = uVelocity ufo
            newHitBox = moveHitBox delta velocity hitBox

moveUfo :: Float -> [UFO] -> [UFO]
moveUfo delta = map moveSingleUfo
  where
    moveSingleUfo ufo = ufo {uHitBox = newHitBox}
      where
        hitBox = uHitBox ufo
        velocity = uVelocity ufo
        newHitBox = moveHitBox delta velocity hitBox



-- spawnBullet position direction gameState = gameState'

shootBulletFromUfo :: Float -> UFO -> GameState -> GameState
shootBulletFromUfo delta ufo@(MkUfo {uShootTime = time}) gameState@(MkGameState {gsSpaceship = spaceship})
  | time < 0  = let gameState' = spawnBullet pos dir gameState in gameState' {gsUfos = updateElement ufo ufo {uShootTime = ufoShootingTimeout} (gsUfos gameState')}
  | otherwise = gameState {gsUfos = updateElement ufo ufo {uShootTime = time - delta} (gsUfos gameState)} 
  where
    uPos  = hPosition $ uHitBox ufo
    uSize = hRadius $ uHitBox ufo
    dir   = normalizeV $ vectorFromPoints (hPosition $ sHitBox spaceship) uPos 
    pos   = translatePoint uPos (mulSV (uSize + 5) dir)
