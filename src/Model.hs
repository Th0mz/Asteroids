module Model where

import qualified Graphics.Gloss as Data (Point, Vector, BitmapData)

-- properties
type Radius = Float
type LifeTime = Float
type Exploding = Bool

-- rendering/physics
type Skin = Data.BitmapData
type Position = Data.Point
type Velocity = Data.Vector
type Acceleration = Data.Vector
data HitBox = MkHitBox Position Radius
data Size = Small | Medium | Large

-- objects
data Spaceship = MkSpaceship Skin HitBox Velocity Acceleration Exploding
data Asteroid = MkAsteroid Skin HitBox Velocity Exploding Size
data Bullet = MkBullet Skin HitBox Velocity LifeTime
data UFO = MkUFO Skin HitBox Velocity Exploding

-- states
type Paused = Bool
type Lives = Int
type Score = Int
type Name = String
type HSEntry = (Name, Score)

data Direction = Up | Down | Left | Right | None
data KeyBoard = Direction | Space | Pause

initialState :: GameState
initialState = MkGameState {
    test = 0,
    test1 = 0
}

data GameState = MkGameState {
    test :: Int,
    test1 :: Int
}


-- initialState :: GameState
-- initialState = MkGameState {
--     spaceship  = Spaceship,
--     asteroids  = [],
--     ufos       = [],
--     bullets    = [],
--     score      = 0,
--     lives      = 3,
--     highScores = loadHighScores "file_name.txt",
--     input      = None,
--     isPaused   = False,
--     -- skins
--     spaceshipSkin = loadSkin "spaceshipSkin.bmp",
--     sAsteroidSkin = loadSkin "sAsteroidSkin.bmp",
--     mAsteroidSkin = loadSkin "mAsteroidSkin.bmp",
--     lAsteroidSkin = loadSkin "lAsteroidSkin.bmp",
--     bulletSkin    = loadSkin "bulletSkin.bmp",
--     ufoSkin       = loadSkin "ufoSkin.bmp"
--     -- animations
-- }

-- data GameState = MkGameState {
--     spaceship  :: Spaceship,
--     asteroids  :: [Asteroid],
--     ufos       :: [UFO],
--     bullets    :: [Bullet],
--     score      :: Score,
--     lives      :: Lives,
--     highScores :: [HSEntry],
--     input      :: KeyBoard,
--     isPaused   :: Paused,
--     -- skins
--     spaceshipSkin :: Skin,
--     sAsteroidSkin :: Skin,
--     mAsteroidSkin :: Skin,
--     lAsteroidSkin :: Skin,
--     bulletSkin    :: Skin,
--     ufoSkin       :: Skin
--     -- animations
-- }