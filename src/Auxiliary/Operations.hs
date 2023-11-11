module Auxiliary.Operations where

import Graphics.Gloss
import Model
import Auxiliary.Constants

-- vector operations
translatePos :: Float -> Point -> Vector -> Point
translatePos delta (x, y) (vX, vY) = (x + vX * delta, y + vY * delta)

translatePoint :: Point -> Vector -> Point
translatePoint (x, y) (vX, vY) = (x + vX, y + vY)

wrapAroundPos :: Float -> Point -> Point
wrapAroundPos slack (x, y) = (wrapAroundCoord x windowMinX windowMaxX, wrapAroundCoord y windowMinY windowMaxY)
    where 
        wrapAroundCoord :: Float -> Float -> Float -> Float
        wrapAroundCoord value min max
            | value <= slackMax && value >= slackMin = value
            | value > slackMax = slackMin
            | otherwise = slackMax
            where 
                slackMin = min - slack
                slackMax = max + slack