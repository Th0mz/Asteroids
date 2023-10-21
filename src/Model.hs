module Model where

import qualified Graphics.Gloss as Data (Point, Vector, BitmapData)
import GHC.Generics (UDouble)
import Data.Data (ConstrRep(FloatConstr))

-- properties
type Radius = Float
type LifeTime = Float
type Exploding = Bool

data HitBox = MkHitBox {
    hPosition :: Data.Point,
    hRadius :: Radius
}

-- objects
type Lives = Int
data Spaceship = MkSpaceship {
    sLives :: Lives,
    sHitBox :: HitBox, 
    sVelocity :: Data.Vector,
    sAcceleration :: Data.Vector,
    sExploding :: Exploding 
}

initSpaceShip :: Spaceship
initSpaceShip = MkSpaceship {
    sLives = 3,
    sHitBox = MkHitBox {hPosition = (0, 0), hRadius = 20},
    sVelocity = (50, 40),
    sAcceleration = (0, 0),
    sExploding = False
}

data Size = Small | Medium | Large
data Asteroid = MkAsteroid {
    aHitBox :: HitBox, 
    aVelocity :: Data.Vector, 
    aExploding :: Exploding, 
    aSize :: Size
}

data Bullet = MkBullet {
    
    bHitBox :: HitBox, 
    bVelocity :: Data.Vector, 
    bLifeTime :: LifeTime
}

data UFO = MkUFO {
    uHitBox :: HitBox, 
    uVelocity :: Data.Vector, 
    uExploding :: Exploding
}

-- general game state
type Paused = Bool
type Score = Int

-- high scores
type Name = String
type HSEntry = (Name, Score)
type HighScores = [HSEntry]

-- TODO: supposed to be at model file?
loadHighScores :: String -> HighScores
loadHighScores _ = [] 

-- player input
data KeyBoard = Up | Down | Left | Right | Space | Pause | None

initialState :: GameState
initialState = MkGameState {
    gsSpaceship = initSpaceShip,
    gsAsteroids = [],
    gsUfos = [],
    gsBullets = [],
    gsScore = 0,
    gsHighScores = loadHighScores "file_name.json",
    gsKeyboard = None,
    gsIsPaused = False
}

data GameState = MkGameState {
    gsSpaceship  :: Spaceship,
    gsAsteroids  :: [Asteroid],
    gsUfos       :: [UFO],
    gsBullets    :: [Bullet],
    gsScore      :: Score,
    gsHighScores :: [HSEntry],
    gsKeyboard   :: KeyBoard,
    gsIsPaused   :: Paused
}