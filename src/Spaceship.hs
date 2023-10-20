-- This module contains the data types which represent the state of the game in the context of the Spaceship
module Spaceship where

import Graphics.Gloss ( red, circle, color, Picture )
import Model (Spaceship)

-- ------------------------------------ --
--              V I E W                 --
-- ------------------------------------ --
renderSpaceship :: Picture
renderSpaceship = color red (circle 50)

-- ------------------------------------ --
--         C O N T R O L L E R          --
-- ------------------------------------ --