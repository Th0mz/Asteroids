module Auxiliary.Operations where

import Graphics.Gloss
import Model
import Auxiliary.Constants

-- vector operations
translatePos :: Float -> Point -> Vector -> Point
translatePos delta (x, y) (vX, vY) = (x + vX * delta, y + vY * delta)

translatePoint :: Point -> Vector -> Point
translatePoint (x, y) (vX, vY) = (x + vX, y + vY)

addVectors :: Vector -> Vector -> Vector
addVectors (vX, vY) (uX, uY) = (vX + uX, vY + uY)

vectorFromPoints :: Point -> Point -> Vector
vectorFromPoints (pX, pY) (uX, uY) = (pX - uX, pY - uX)

scaleVector :: Float -> Vector -> Vector
scaleVector factor (vX, vY) = (vX - vX * factor, vY - vY * factor)

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