module Main where

import Controller
import Model.Asteroid
import Model.Spaceship
import Model.UFO
import View
import Model.Model

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
