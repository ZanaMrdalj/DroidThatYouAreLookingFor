module GameState (
    GameState(..)
    , getMenu
    , getPlay
    , getGameOver
    , getYouWin ) 
    where

import Graphics.Gloss
import LivePicture
import Card
import qualified Graphics.Gloss.Game as GlossGame
import System.Random
import System.Random.Shuffle


-- tip podataka koji opisuje stanja u kojima igra moze da se nalazi
data GameState = Intro
    { currentTime :: Int -- current system time since the game was started
    , duration :: Float  -- trajanje Intro stanja u sekundama
    , timePassed :: Float  -- vreme proteklo od prethodnog frejma
    , introPicture :: Picture } 
    | Menu 
    { currentTime :: Int
    , newGame :: LivePicture 
    , quitGame :: LivePicture } 
    | Play
    { currentTime :: Int  
    , clickNumber :: Int
    , duration :: Float  -- vreme koje imamo na raspolaganju do zavrsetka igre
    , timePassed :: Float
    , cards :: [Card] 
    , matchingCards :: (Maybe Card, Maybe Card)   -- uredjeni par gde cuvamo podatke o kartama koje cemo porediti
    }
    | GameOver 
    {   duration :: Float
    ,   timePassed :: Float
    ,   endGame :: Picture  
    }
    | YouWin
    {   duration :: Float
    ,   timePassed :: Float  
    ,   congrats :: Picture
    }    
        deriving Show


getMenu :: Int -> GameState 
getMenu time = Menu 
            { currentTime = time
            , newGame = LivePicture.create (GlossGame.png ".\\assets\\newGame.png") 500 125 (0 , 50)  
            , quitGame = LivePicture.create (GlossGame.png ".\\assets\\quitGame.png") 500 125 (0 , -50) 
            }

getPlay :: Int -> GameState
getPlay time = Play
            { currentTime = time
            , clickNumber = 0
            , duration = 100
            , timePassed = 0
            , matchingCards = (Nothing, Nothing)
            , cards =  [ Card.createCard (GlossGame.png ".\\assets\\cards\\bb8.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 0) 0
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\bb8.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 1) 0
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\ivy3.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 2) 1
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\ivy3.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 3) 1
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\wallee2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 4) 2
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\wallee2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 5) 2
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\android.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 ((coordinates !! 6)) 3  
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\android.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 7) 3  
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\bigHero6.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 8) 4
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\bigHero6.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 9) 4
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\battledroid2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 10) 5
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\battledroid2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 11) 5
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\r2d2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 12) 6
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\r2d2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 13) 6
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\reaper2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 14) 7
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\reaper2.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 15) 7
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\3cpo3.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 16) 8
                      , Card.createCard (GlossGame.png ".\\assets\\cards\\3cpo3.png") (GlossGame.png ".\\assets\\cards\\cardBack.png") 75 125 (coordinates !! 17) 8   
                      ]
            }
            where
                startCoordinates :: [Position]
                startCoordinates = [ (-160, 0), (-80, 0), (0,0), (80, 0)
                                    , (160, 0), (240, 0), (-160, 130), (-80, 130)
                                    , (0, 130), (80, 130), (160, 130), (240, 130)
                                    , (-160, -130), (-80, -130) , (0, -130), (80, -130)
                                    , (160, -130), (240, -130) 
                                    ]
                randomGenerator = mkStdGen time
                coordinates = shuffle' startCoordinates (length startCoordinates) randomGenerator  --randomiziranje niza sa koordinatama                                 

getGameOver :: GameState
getGameOver = GameOver
                    { duration = 3
                    , timePassed = 0
                    , endGame = GlossGame.png ".\\assets\\gameover.png"
                    }

getYouWin :: GameState
getYouWin = YouWin
            { duration = 3
            , timePassed = 0
            , congrats = GlossGame.png ".\\assets\\youwin.png"
            }
                   