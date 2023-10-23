--This module defines how to turn the gamestate into a picture (incl. animations)
module View where

import Graphics.Gloss
import Model
import Asteroid
import Spaceship
import UFO
    -- ( GameState(infoToShow),
    --  InfoToShow(ShowAChar, ShowNothing, ShowANumber))

-- view :: GameState -> IO Picture
-- -- view = return . viewPure
-- view MkGameState {gsSpaceship = spaceship} = do
--     spaceshipPicture <- renderSpaceship spaceship
--     spaceshipPictureHB <- renderSpaceshipHB spaceship

--     return $ Pictures [spaceshipPicture, spaceshipPictureHB]


view :: GameState -> IO Picture
view gameState = do
    spaceshipPicture   <- renderSpaceship (gsSpaceship gameState)
    spaceshipPictureHB <- renderSpaceshipHB (gsSpaceship gameState)
    asteroidPicture    <- renderAsteroid asteroidToRender
    asteroidPictureHB  <- renderAsteroidHB asteroidToRender
   
    return $ Pictures
        [ spaceshipPicture
        , spaceshipPictureHB
        , asteroidPicture
        , asteroidPictureHB
        ]
    where
        asteroidToRender = case gsAsteroids gameState of
            (asteroid:_) -> asteroid
            _ -> defaultAsteroid --define!!!

    
viewPure :: GameState -> Picture
-- viewPure gsate = case infoToShow gstate of
--     ShowNothing   -> blank
--     ShowANumber n -> color green (text (show n))
--     ShowAChar   c -> color green (text [c])
viewPure = undefined
