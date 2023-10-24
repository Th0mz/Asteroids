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


