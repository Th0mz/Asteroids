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

