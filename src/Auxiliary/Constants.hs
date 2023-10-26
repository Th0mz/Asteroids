module Auxiliary.Constants where

framesPerSecond :: Int
framesPerSecond = 10

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