module Auxiliary.Constants where

framesPerSecond :: Int
framesPerSecond = 60

-- -------------------- --
--     C A N V A S      --
-- -------------------- --

-- width
windowWidth :: Int
windowWidth = 800

windowMinX :: Float
windowMinX = fromIntegral (- windowWidth `div` 2)
windowMaxX :: Float
windowMaxX = fromIntegral   (windowWidth `div` 2)


-- height
windowHeight :: Int
windowHeight = 600

windowMinY :: Float
windowMinY = fromIntegral (- windowHeight `div` 2)
windowMaxY :: Float
windowMaxY = fromIntegral   (windowHeight `div` 2)

-- -------------------- --
--      P A T H S       --
-- -------------------- --
bitmapPaths :: String
bitmapPaths = "./bitmaps"

spaceshipBitmap :: String
spaceshipBitmap = bitmapPaths ++ "/spaceshipSkin.bmp"

spaceshipSize :: Float
spaceshipSize = 32

sAsteroidBitmap :: String
sAsteroidBitmap = bitmapPaths ++ "/sAsteroidSkin.bmp"

sAsteroidSize :: Float
sAsteroidSize = 30

mAsteroidBitmap :: String
mAsteroidBitmap = bitmapPaths ++ "/mAsteroidSkin.bmp"

mAsteroidSize :: Float
mAsteroidSize = 60

lAsteroidBitmap :: String
lAsteroidBitmap = bitmapPaths ++ "/lAsteroidSkin.bmp"

lAsteroidSize :: Float
lAsteroidSize = 100

ufoBitmap :: String
ufoBitmap = bitmapPaths ++ "/ufoSkin.bmp"

ufoSize :: Float
ufoSize = 60


-- ---------------------------- --
--      S P A C E S H I P       --
-- ---------------------------- --
spaceshipRotationSpeed :: Float
spaceshipRotationSpeed = 4

spaceshipAcceleration :: Float
spaceshipAcceleration = 100


-- friction coefficient 
--    0 => no friction
--    1 => max friction 
spaceshipFriction :: Float
spaceshipFriction = 0.7

spaceshipMaxSpeed :: Float
spaceshipMaxSpeed = 300

shootingCoolDown :: Float 
shootingCoolDown = 0.2

-- ---------------------------- --
--        B U L L E T           --
-- ---------------------------- --
bulletRadius :: Float
bulletRadius = 2

bulletSpeed :: Float
bulletSpeed = 400

bulletLifetime :: Float
bulletLifetime = 1

-- ---------------------------- --
--       A S T E R O I D        --
-- ---------------------------- --
sAsteroidSpeed :: Float
sAsteroidSpeed = 100

mAsteroidSpeed :: Float
mAsteroidSpeed = 50

lAsteroidSpeed :: Float
lAsteroidSpeed = 25

sAsteroidScore :: Int
sAsteroidScore = 500

mAsteroidScore :: Int
mAsteroidScore = 250

lAsteroidScore :: Int
lAsteroidScore = 100

-- ---------------------------- --
--            U F O             --
-- ---------------------------- --
timeBetweenUFOs :: Float
timeBetweenUFOs = 15

ufoSpeed :: Float
ufoSpeed = 50

ufoScore :: Int
ufoScore = 1000

ufoShootingTimeout :: Float
ufoShootingTimeout = 1.5

-- ---------------------------- --
--      H I G H S C O R E S     --
-- ---------------------------- --
playerName :: String
playerName = "PLY"

highScoreSize :: Int
highScoreSize = 5

highScoresPath :: String
highScoresPath = "highscores.txt"
