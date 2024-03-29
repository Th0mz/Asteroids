module Main where

import Controller
import Asteroid
import Spaceship
import UFO
import View
import Model
import Auxiliary.Constants
import System.IO

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
       gameState <- initialState 
       playIO (InWindow "Asteroids" (windowWidth, windowHeight) (0, 0))
              black
              framesPerSecond
              gameState
              view
              input
              step
