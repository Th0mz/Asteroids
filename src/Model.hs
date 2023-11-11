module Model where

import qualified Graphics.Gloss as Data (Point, Vector, loadBMP)
import GHC.Generics (UDouble)
import Data.Data (ConstrRep(FloatConstr))
import Graphics.Gloss (Picture, Point)
import System.Random
import Auxiliary.Constants
import qualified Data.Set as S

-- properties
type Radius = Float
type LifeTime = Float
type Exploding = Bool

-- point operations
pDistance :: Point -> Point -> Float
pDistance (x, y) (x', y') = sqrt $ (x' - x) ** 2 + (y' - y) ** 2

class Collidable a where
    getHitBox   :: a -> HitBox  
    collided    :: a -> GameState -> GameState
    isColliding :: Collidable b => a -> b -> Bool
    isColliding x y = pDistance posX posY < rX + rY
        where 
            -- x object information 
            hitBoxX = getHitBox x
            posX = hPosition hitBoxX
            rX = hRadius hitBoxX
            -- y object information
            hitBoxY = getHitBox y
            posY = hPosition hitBoxY
            rY = hRadius hitBoxY


data HitBox = MkHitBox {
    hPosition :: Data.Point,
    hRadius :: Radius
}

-- spaceship
type Lives = Int
data Spaceship = MkSpaceship {
    sSkin :: IO Picture,
    sLives :: Lives,
    sHitBox :: HitBox, 
    sDirection :: Data.Vector,
    sVelocity :: Data.Vector,
    sAcceleration :: Data.Vector,
    sExploding :: Exploding 
}

initSpaceShip :: Spaceship
initSpaceShip = MkSpaceship {
    sSkin = Data.loadBMP spaceshipBitmap,
    sLives = 3,
    sHitBox = MkHitBox {hPosition = (0, 0), hRadius = spaceshipSize / 2},
    sDirection = (0, 1),
    sVelocity = (0, 0),
    sAcceleration = (0, 0),
    sExploding = False
}

-- asteroid
data Size = Small | Medium | Large
data Asteroid = MkAsteroid {
    aSkin :: IO Picture,
    aHitBox :: HitBox, 
    aVelocity :: Data.Vector, 
    aExploding :: Exploding, 
    aSize :: Size
}

initAsteroid :: Size -> IO Asteroid
initAsteroid size = do
    randomX <- randomRIO (windowMinX, windowMaxX)
    randomY <- randomRIO (windowMinY, windowMaxY)
    return MkAsteroid {
        aSkin = Data.loadBMP $ case size of
            Small -> sAsteroidBitmap
            Medium -> mAsteroidBitmap
            Large -> lAsteroidBitmap,
        aHitBox = MkHitBox { hPosition = (randomX, randomY), hRadius = case size of
            Small -> sAsteroidSize / 2
            Medium -> mAsteroidSize / 2
            Large -> lAsteroidSize / 2
      },
        aVelocity = (20, 20),
        aExploding = False,
        aSize = size
    }

addAsteroid :: Asteroid -> [Asteroid] -> [Asteroid]
addAsteroid asteroid asteroids = asteroid : asteroids

-- bullet
data Bullet = MkBullet {
    bHitBox :: HitBox, 
    bVelocity :: Data.Vector, 
    bLifeTime :: LifeTime
}

initBullet :: Bullet --not yet correct, random values assigned
initBullet = MkBullet {
    bHitBox = MkHitBox { hPosition = (0,0), hRadius = 1},
    bVelocity = (100, 100),
    bLifeTime = 1
}

addBullet :: Bullet -> [Bullet] -> [Bullet]
addBullet bullet bullets = bullet : bullets

-- ufo
data UFO = MkUfo {
    uSkin :: IO Picture,
    uHitBox :: HitBox, 
    uVelocity :: Data.Vector, 
    uExploding :: Exploding
}

initUfo :: IO UFO
initUfo = do
    randomX <- randomRIO (windowMinX, windowMaxX)
    randomY <- randomRIO (windowMinY, windowMaxY)
    return MkUfo {
        uSkin = Data.loadBMP ufoBitmap,
        uHitBox = MkHitBox { hPosition = (randomX, randomY), hRadius = ufoSize / 2 },
        uVelocity = (30, 30),
        uExploding = False
    }

addUfo :: UFO -> [UFO] -> [UFO]
addUfo ufo ufos = ufo : ufos

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
type Keys = S.Set KeyBoard
data KeyBoard = KBup | KBleft | KBright | KBspace | KBpause | KBnone
    deriving (Eq, Ord)


initialState :: IO GameState
initialState = do
    randomSmallAsteroid  <- initAsteroid Small
    randomMediumAsteroid <- initAsteroid Medium
    randomLargeAsteroid  <- initAsteroid Large
    randomUfo            <- initUfo

    return MkGameState {
        gsSpaceship = initSpaceShip,
        gsAsteroids = [randomSmallAsteroid, randomMediumAsteroid, randomLargeAsteroid],
        gsUfos = [randomUfo],
        gsBullets = [],
        gsScore = 0,
        gsHighScores = loadHighScores "file_name.json",
        gsKeys = S.empty,
        gsIsPaused = False
    }

data GameState = MkGameState {
    gsSpaceship  :: Spaceship,
    gsAsteroids  :: [Asteroid],
    gsUfos       :: [UFO],
    gsBullets    :: [Bullet],
    gsScore      :: Score,
    gsHighScores :: [HSEntry],
    gsKeys       :: Keys,
    gsIsPaused   :: Paused
}