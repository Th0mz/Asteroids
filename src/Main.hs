module Main where

import Controller
import Asteroid
import Spaceship
import UFO
import View
import Model
import Auxiliary.Constants

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (windowWidth, windowHeight) (0, 0)) -- Or FullScreen
              black            -- Background color
              framesPerSecond  -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
