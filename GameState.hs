module GameState (
    GameState(..)
    , getMenu
    , getPlay
    , getGameOver) 
    where

import Graphics.Gloss
import LivePicture
import Card
import qualified Graphics.Gloss.Game as GlossGame

data GameState = Intro
    { duration :: Float  -- duration of Intro in seconds
    , timePassed :: Float
    , introPicture :: Picture } 
    | Menu 
    { newGame :: LivePicture 
    , quitGame :: LivePicture } 
    | Play 
    { duration :: Float
    , timePassed :: Float
    , cards :: [Card]    
    }
    | GameOver 
    {   duration :: Float
    ,   timePassed :: Float
    ,   endGame :: Picture  
    }
        deriving Show


getMenu :: GameState
getMenu = Menu 
            { newGame = LivePicture.create (GlossGame.png ".\\assets\\newGame.png") 500 125 (0 , 50)  
            , quitGame = LivePicture.create (GlossGame.png ".\\assets\\quitGame.png") 500 125 (0 , -50) 
            }


getPlay :: GameState
getPlay = Play
            { duration = 5
            , timePassed = 0
            , cards =  [ Card.createCard (GlossGame.png ".\\assets\\cards\\bb8.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-160, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\bb8.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-80, 0)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\ivy3.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (0, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\ivy3.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (80, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\wallee2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (160, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\wallee2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (240, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\android.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-160, 130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\android.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-80, 130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\bigHero6.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (0, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\bighero6.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (80, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\battledroid2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (160, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\battledroid2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (240, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\r2d2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-160, -130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\r2d2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-80, -130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\reaper2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (0, -130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\reaper2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (80, -130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (160, -130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (240, -130)   
                      ]
            }            


getGameOver :: GameState
getGameOver = GameOver
                    { duration = 2
                    , timePassed = 0
                    , endGame = GlossGame.png ".\\assets\\gameover.png"
                    }

                   