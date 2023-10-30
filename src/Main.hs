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
main = initialState >>= \initialGameState ->
       playIO (InWindow "Asteroids" (windowWidth, windowHeight) (0, 0))
              black
              framesPerSecond
              initialGameState
              view
              input
              step
