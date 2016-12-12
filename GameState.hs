module GameState (GameState(..), getMenu, getPlay) where

import Graphics.Gloss
import LivePicture
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
    }
    | GameOver deriving Show


newGamePicture :: Picture
newGamePicture = translate 0 50 $ GlossGame.png ".\\assets\\newGame.png"
quitGamePicture :: Picture
quitGamePicture = translate 0 (-50) $ GlossGame.png ".\\assets\\quitGame.png"

getMenu :: GameState
getMenu = Menu 
            { newGame = LivePicture { picture = newGamePicture, width = 500, height = 125, position = (0 , 50)  }
            , quitGame = LivePicture { picture = quitGamePicture, width = 500, height = 125, position = (0 , -50) }
            }

getPlay :: GameState
getPlay = Play
            { duration = 120
            , timePassed = 0
            }            
