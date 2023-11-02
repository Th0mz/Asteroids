--This module defines how to turn the gamestate into a picture (incl. animations)
module View where

import Graphics.Gloss
import Model
import Asteroid
import Spaceship
import UFO
import Bullet (renderBullet)
    -- ( GameState(infoToShow),
    --  InfoToShow(ShowAChar, ShowNothing, ShowANumber))

view :: GameState -> IO Picture
view gameState = do
    spaceshipPicture   <- renderSpaceship       (gsSpaceship gameState)
    spaceshipPictureHB <- renderSpaceshipHB     (gsSpaceship gameState)
    asteroidPictures   <- mapM renderAsteroid   (gsAsteroids gameState)
    asteroidPicturesHB <- mapM renderAsteroidHB (gsAsteroids gameState)
    ufoPictures        <- mapM renderUfo        (gsUfos gameState)
    ufoPicturesHB      <- mapM renderUfoHB      (gsUfos gameState)
    bulletPictures     <- mapM renderBullet     (gsBullets gameState)
   
    return $ Pictures $
        [  spaceshipPicture, 
           spaceshipPictureHB ] 
        ++ asteroidPictures
        ++ asteroidPicturesHB
        ++ ufoPictures
        ++ ufoPicturesHB
        ++ bulletPictures
    
viewPure :: GameState -> Picture
-- viewPure gsate = case infoToShow gstate of
--     ShowNothing   -> blank
--     ShowANumber n -> color green (text (show n))
--     ShowAChar   c -> color green (text [c])
viewPure = undefined
