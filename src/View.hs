--This module defines how to turn the gamestate into a picture (incl. animations)
module View where

import System.IO
import Graphics.Gloss
import Model
import Asteroid
import Spaceship
import UFO
import HighScores
import Bullet (renderBullet)
import Auxiliary.Constants (windowWidth, windowHeight)
    -- ( GameState(infoToShow),
    --  InfoToShow(ShowAChar, ShowNothing, ShowANumber))

view :: GameState -> IO Picture
view gameState@(MkGameState {gsScreen = screen}) =
    case screen of
        Main       -> mainView gameState
        Game       -> gameView gameState
        Pause      -> pauseView gameState
        HighScores -> highScoresView gameState

-----------------------------------------
--              M A I N                --
-----------------------------------------
mainView :: GameState -> IO Picture
mainView gameState = let
    asteroidsTitle = Translate (-225) 0 $ Scale 0.7 0.7 $ Color white $ Text "ASTEROIDS"
    instrText = Translate (-200) (-40) $ Scale 0.2 0.2 $ Color white $ Text "press 'enter' to start the game"

    in return $ Pictures [asteroidsTitle, instrText]

-----------------------------------------
--              G A M E                --
-----------------------------------------
gameView :: GameState -> IO Picture
gameView gameState@(MkGameState {gsScore = score, gsSpaceship = spaceship}) = do
    spaceshipPicture   <- renderSpaceship       (gsSpaceship gameState)
    asteroidPictures   <- mapM renderAsteroid   (gsAsteroids gameState)
    ufoPictures        <- mapM renderUfo        (gsUfos gameState)
    bulletPictures     <- mapM renderBullet     (gsBullets gameState)
    let scoreText = Translate (- fromIntegral windowWidth / 2 + 10) (fromIntegral windowHeight / 2 - 25) $ Scale 0.15 0.15 $ Color white $ Text $ "score: " ++ show score 
        livesText = Translate (- fromIntegral windowWidth / 2 + 10) (fromIntegral windowHeight / 2 - 50) $ Scale 0.15 0.15 $ Color white $ Text $ "lives: " ++ show (sLives spaceship)

    return $ Pictures $
        spaceshipPicture : 
        asteroidPictures
        ++ ufoPictures
        ++ bulletPictures
        ++ [ scoreText,
             livesText ]

-----------------------------------------
--             P A U S E               --
-----------------------------------------
pauseView :: GameState -> IO Picture
pauseView gameState = do
    game <- gameView gameState
    let pauseText = Translate (-90) 0 $ Scale 0.5 0.5 $ Color white $ Text "Pause"
        instrText = Translate (-200) (-40) $ Scale 0.2 0.2 $ Color white $ Text "press 'p' again to resume play"
    return $ Pictures $ game : [pauseText, instrText]

-----------------------------------------
--         H I G H S C O R E S         --
-----------------------------------------
highScoresView :: GameState -> IO Picture
highScoresView gameState@(MkGameState {gsHighScores = highscores}) = do
    let highscText = Translate (-200) (fromIntegral windowHeight / 2 - 100) $ Scale 0.5 0.5 $ Color white $ Text $ "High Scores"
        quitText  = Translate (-125) (-80) $ Scale 0.2 0.2 $ Color white $ Text "press 'q' to quit" --still have to fix this
        playText  = Translate (-145) (-120) $ Scale 0.2 0.2 $ Color white $ Text "press enter to quit" --still have to fix this
    
    return $ Pictures [renderHighScores highscores, highscText, quitText, playText]

renderHighScores :: HighScores -> Picture
renderHighScores highScores =
    let renderedScores = zipWith (curry renderScoreWithPosition) [1..] highScores
    in Pictures renderedScores

renderScoreWithPosition :: (Int, HSEntry) -> Picture
renderScoreWithPosition (position, (name, score)) =
    Translate (-100) (fromIntegral (170 + (-40 * position))) $
    Scale 0.2 0.2 $
    Color white $
    Text $ name ++ ": " ++ show score