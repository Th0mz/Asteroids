-- This module contains the data types which represent the state of the game in the context of the UFO
module Model.UFO where

import qualified Graphics.Gloss as Data (Point, Vector, BitmapData)

data InfoToShow = ShowNothing
                | ShowANumber Int
                | ShowAChar   Char

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data Bullet = MkBullet Skin HitBox Velocity LifeTime
data UFO = MkUFO Skin HitBox Velocity Exploding