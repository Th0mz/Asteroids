{-# LANGUAGE InstanceSigs #-}
module Model where

import qualified Graphics.Gloss as Data (Point, Vector, loadBMP)
import GHC.Generics (UDouble)
import Data.Data (ConstrRep(FloatConstr))
import Graphics.Gloss (Picture, Point)
import System.Random
import Auxiliary.Constants
import qualified Data.Set as S
import HighScores
import Data.List (elemIndex)
import Data.Bits (Bits(xor))
import qualified Data.Maybe
import Data.Map (elemAt)

-- properties
type Radius = Float
type LifeTime = Float
type Collided = Bool
type Cooldown = Float
type Identifier = Int

-- point operations
pDistance :: Point -> Point -> Float
pDistance (x, y) (x', y') = sqrt $ (x' - x) ** 2 + (y' - y) ** 2


------------------------------------------
--          C O L L I D A B L E         --
------------------------------------------
class Collidable a where
    getHitBox   :: a -> HitBox
    didCollide  :: a -> Collided
    removeCollieded :: a -> a
    afterCollision    :: a -> GameState -> GameState
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


-- collidable instances
instance Collidable Spaceship where
    getHitBox = sHitBox
    didCollide = sCollided

    afterCollision :: Spaceship -> GameState -> GameState
    afterCollision spaceship@(MkSpaceship {sLives = lives, sHitBox = hitBox}) gameState
        | not $ sCollided spaceship = gameState {gsSpaceship = newSpaceship}
        | otherwise = gameState
        where
            newHitBox = hitBox {hPosition = (0, 0)}
            newSpaceship = spaceship {
                sVelocity  = (0, 0),
                sDirection = (0, 1),
                sHitBox = newHitBox,
                sLives = lives - 1,
                sCollided = True
            }

    removeCollieded :: Spaceship -> Spaceship
    removeCollieded spaceship = spaceship {sCollided = False}


instance Collidable Asteroid where
    getHitBox = aHitBox
    didCollide = aCollided

    afterCollision :: Asteroid -> GameState -> GameState
    afterCollision _  = id

    removeCollieded :: Asteroid -> Asteroid
    removeCollieded asteroid = asteroid {aCollided = False}


instance Collidable UFO where
    getHitBox = uHitBox
    didCollide = uCollided
    afterCollision = undefined

    removeCollieded :: UFO -> UFO
    removeCollieded ufo = ufo {uCollided = False}

instance Collidable Bullet where
    getHitBox = bHitBox
    didCollide = bCollided

    afterCollision :: Bullet -> GameState -> GameState
    afterCollision bullet gameState@(MkGameState {gsBullets = bullets}) = 
        gameState {
            gsBullets = updateIndex index bullet { bLifeTime = 0 } bullets
        }
        where
            index = case elemIndex bullet bullets of
                Just i -> i
                Nothing -> error "bullet not in bullets" 


            


    removeCollieded :: Bullet -> Bullet
    removeCollieded bullet = bullet {bCollided = False}

updateIndex :: Int -> a -> [a] -> [a]
updateIndex i newVal lst =  take i lst ++ [newVal] ++ drop (i + 1) lst


data HitBox = MkHitBox {
    hPosition :: Data.Point,
    hRadius :: Radius
}
-- spaceship
type Lives = Int
data Spaceship = MkSpaceship {
    sId   :: Identifier,
    sSkin :: IO Picture,
    sLives :: Lives,
    sCooldown :: Cooldown,
    sHitBox :: HitBox,
    sDirection :: Data.Vector,
    sVelocity :: Data.Vector,
    sCollided :: Collided
}

instance Eq Spaceship where
    MkSpaceship{sId = idX} == MkSpaceship{sId = idY} = idX == idY

-- asteroid
data Size = Small | Medium | Large
data Asteroid = MkAsteroid {
    aId   :: Identifier,
    aSkin :: IO Picture,
    aHitBox :: HitBox,
    aVelocity :: Data.Vector,
    aCollided :: Collided,
    aSize :: Size
}

instance Eq Asteroid where
    MkAsteroid{aId = idX} == MkAsteroid{aId = idY} = idX == idY

-- bullet
data Bullet = MkBullet {
    bId   :: Identifier,
    bHitBox :: HitBox,
    bVelocity :: Data.Vector,
    bLifeTime :: LifeTime,
    bCollided :: Collided
}

instance Eq Bullet where
    MkBullet{bId = idX} == MkBullet{bId = idY} = idX == idY

-- ufo
data UFO = MkUfo {
    uId   :: Identifier,
    uSkin :: IO Picture,
    uHitBox :: HitBox,
    uVelocity :: Data.Vector,
    uCollided :: Collided
}

instance Eq UFO where
    MkUfo{uId = idX} == MkUfo{uId = idY} = idX == idY

-- general game state
type Paused = Bool
type Score = Int


data Screen = Main | Game | Pause | HighScores

-- player input
type Keys = S.Set KeyBoard
data KeyBoard = KBup | KBleft | KBright | KBspace | KBenter | KBpause | KBnone
    deriving (Eq, Ord)


getIdentifier :: GameState -> (Identifier, GameState)
getIdentifier gameState@(MkGameState {gsGlobalIdentifier = id}) =
    (id, gameState {gsGlobalIdentifier = id + 1}) 

initialState :: IO GameState
initialState = do
    highScores           <- loadHighScores "high-scores.txt"

    return MkGameState {
        gsScreen = Main,
        gsSpaceship = undefined,
        gsAsteroids = [],
        gsUfos = [],
        gsBullets = [],
        gsScore = 0,
        gsHighScores = highScores,
        gsKeys = S.empty,
        gsIsPaused = False,
        gsGlobalIdentifier = 0,

        -- skins
        gsSpaceshipSkin = Data.loadBMP spaceshipBitmap,
        gsAsteroidSkinS = Data.loadBMP sAsteroidBitmap,
        gsAsteroidSkinM = Data.loadBMP mAsteroidBitmap,
        gsAsteroidSkinL = Data.loadBMP lAsteroidBitmap,
        gsUFOSkin       = Data.loadBMP ufoBitmap
    }

data GameState = MkGameState {
    gsScreen     :: Screen,
    gsSpaceship  :: Spaceship,
    gsAsteroids  :: [Asteroid],
    gsUfos       :: [UFO],
    gsBullets    :: [Bullet],
    gsScore      :: HighScores.Score,
    gsHighScores :: [HSEntry],
    gsKeys       :: Keys,
    gsIsPaused   :: Paused,
    gsGlobalIdentifier :: Identifier,

    -- skins
    gsSpaceshipSkin  :: IO Picture,
    gsAsteroidSkinS  :: IO Picture,
    gsAsteroidSkinM  :: IO Picture,
    gsAsteroidSkinL  :: IO Picture,
    gsUFOSkin        :: IO Picture
}
