module Auxiliary.Operations where

import Graphics.Gloss
import Model
import Auxiliary.Constants


-- hitBox operations
moveHitBox :: Float -> Vector -> HitBox -> HitBox
moveHitBox delta velocity hitBox = 
    hitBox {hPosition = newPosition}
    where 
        position = hPosition hitBox
        radius = hRadius hitBox
        newPosition = wrapAroundPos radius $ translatePos delta position velocity

-- vector operations
translatePos :: Float -> Point -> Vector -> Point
translatePos delta (x, y) (vX, vY) = (x + vX * delta, y + vY * delta)

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