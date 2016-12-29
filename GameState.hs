module GameState (
    GameState(..)
    , getMenu
    , getPlay) 
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
    | GameOver deriving Show


getMenu :: GameState
getMenu = Menu 
            { newGame = LivePicture.create (GlossGame.png ".\\assets\\newGame.png") 500 125 (0 , 50)  
            , quitGame = LivePicture.create (GlossGame.png ".\\assets\\quitGame.png") 500 125 (0 , -50) 
            }

getPlay :: GameState
getPlay = Play
            { duration = 120
            , timePassed = 0
            , cards =  [ Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-160, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-80, 0)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (0, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (80, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (160, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (240, 0)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-160, 130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-80, 130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (0, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (80, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (160, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (240, 130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-160, -130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (-80, -130)   
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (0, -130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (80, -130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (160, -130)
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\droid1.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (240, -130)   
                      ]
            }            

