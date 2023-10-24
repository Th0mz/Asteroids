module Model where

import qualified Graphics.Gloss as Data (Point, Vector, loadBMP)
import GHC.Generics (UDouble)
import Data.Data (ConstrRep(FloatConstr))
import Graphics.Gloss (Picture)
import Auxiliary.Constants

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
    sSkin :: IO Picture,
    sLives :: Lives,
    sHitBox :: HitBox, 
    sVelocity :: Data.Vector,
    sAcceleration :: Data.Vector,
    sExploding :: Exploding 
}

initSpaceShip :: Spaceship
initSpaceShip = MkSpaceship {
    sSkin = Data.loadBMP spaceshipBitmap,
    sLives = 3,
    sHitBox = MkHitBox {hPosition = (0, 0), hRadius = spaceshipSize / 2},
    sVelocity = (50, 40),
    sAcceleration = (0, 0),
    sExploding = False
}

data Size = Small | Medium | Large
data Asteroid = MkAsteroid {
    aSkin :: IO Picture,
    aHitBox :: HitBox, 
    aVelocity :: Data.Vector, 
    aExploding :: Exploding, 
    aSize :: Size
}

initAsteroid :: Size -> Asteroid
initAsteroid size = MkAsteroid {
    aSkin = Data.loadBMP $ case size of
        Small -> sAsteroidBitmap
        Medium -> mAsteroidBitmap
        Large -> lAsteroidBitmap,
    aHitBox = MkHitBox {hPosition = case size of --here, a random starting position should be implemented, not fixed values!!!
        Small -> (40, 80)
        Medium -> (200,100)
        Large -> (650, 450),
    hRadius = case size of
        Small -> sAsteroidSize / 2
        Medium -> mAsteroidSize / 2
        Large -> lAsteroidSize / 2
    },
    aVelocity = (20, 20),
    aExploding = False,
    aSize = size
}

data Bullet = MkBullet {
    bHitBox :: HitBox, 
    bVelocity :: Data.Vector, 
    bLifeTime :: LifeTime
}

data UFO = MkUfo {
    uSkin :: IO Picture,
    uHitBox :: HitBox, 
    uVelocity :: Data.Vector, 
    uExploding :: Exploding
}

initUfo :: UFO
initUfo = MkUfo {
    uSkin = Data.loadBMP ufoBitmap,
    uHitBox = MkHitBox {hPosition = (80, 80), hRadius = ufoSize / 2}, --also here, random starting position
    uVelocity = (30, 30),
    uExploding = False
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
    gsAsteroids = [initAsteroid Small, initAsteroid Medium, initAsteroid Large],
    gsUfos = [initUfo], --start with empty list, no asteroids
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