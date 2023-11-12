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
view gameState@(MkGameState {gsScreen = screen}) =
    case screen of
        Main       -> mainView gameState
        Game       -> gameView gameState
        Pause      -> pauseView gameState
        HighScores -> highScoresView gameState

-- main screen view
mainView :: GameState -> IO Picture
mainView gameState = undefined

-- game screen view
gameView :: GameState -> IO Picture
gameView gameState = do
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

-- pause screen view
pauseView :: GameState -> IO Picture
pauseView gameState = do
    game <- gameView gameState
    let pauseText = Translate (-90) 0 $ Scale 0.5 0.5 $ Color white $ Text "Pause"
        instrText = Translate (-200) (-40) $ Scale 0.2 0.2 $ Color white $ Text "press 'p' again to resume play"
    return $ Pictures $ game : [pauseText, instrText]

-- high scores view
highScoresView :: GameState -> IO Picture
highScoresView gameState = undefined