--This module defines how to turn the gamestate into a picture (incl. animations)
module View where

import Graphics.Gloss
import Model.Asteroid
import Model.Spaceship
import Model.UFO
    ( GameState(infoToShow),
      InfoToShow(ShowAChar, ShowNothing, ShowANumber))

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gsate = case infoToShow gstate of
    ShowNothing   -> blank
    ShowANumber n -> color green (text (show n))
    ShowAChar   c -> color green (text [c])

