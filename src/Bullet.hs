module Bullet where

import Model

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --

renderBulletHB :: Bullet -> IO Picture
renderBulletHB bullet = undefined

renderBullet :: Bullet -> IO Picture
renderBulletHB bullet = undefined

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --

-- bullet garbage collector must be done here
stepBullets :: Float -> GameState -> GameState
stepBullets delta gameState@(MkGameState {gsBullets = bullets}) =
    gameState {gsBullets = map moveBullet bullets}

moveBullet :: Float -> Bullet -> Bullet
moveBullet delta bullet = undefined

isBulletAlive :: Bullet -> Maybe Bullet
isBulletAlive = undefined

