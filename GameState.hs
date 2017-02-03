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


-- | 'GameState' represents possible states of the game.
data GameState = Intro -- | 'Intro' game state, loads at the game start and lasts for a predefined time.
    { currentTime :: Int -- | Current system time.
    , duration :: Float  -- | Duration of the Intro state.
    , timePassed :: Float  -- | Total time passed since the begining of the state.
    , introPicture :: Picture  -- | Intro image that is rendered on the screen.
    } 
    | Menu -- | 'Menu' game state. Loads automaticaly after the intro. Gives the player options to start a new game or quit the game.
    { currentTime :: Int -- | Current system time.
    , newGame :: LivePicture -- | clickable LivePicture, initiates play state.
    , quitGame :: LivePicture  -- | clickable LivePicture, exits the game.
    } 
    | Play -- | 'Play' game state. Represents the state in which the user can play the memory game.
    { currentTime :: Int -- | Current system time.
    , clickNumber :: Int -- | Used for tracking of the clicked cards.
    , duration :: Float  -- | Time available to the player to finish the game. On exparation, player looses the game.
    , timePassed :: Float -- | Time passed in the play state. Used for timer calculations.
    , cards :: [Card] -- | List of cards availavle to the player.
    , matchingCards :: (Maybe Card, Maybe Card)   -- | Used for comparison of the cards flipped by the player.
    }
    | GameOver -- | 'Game Over' game state. Executed in case of the time exparation in play state.
    {   duration :: Float -- | Duration of the 'Game Over' game state.
    ,   timePassed :: Float -- | Time passed in the 'Game Over' state.
    ,   endGame :: Picture  -- | Image rendered in the Game Over state.
    }
    | YouWin -- | 'Win' game state. Executed when the player succesfully matches all the cards.
    {   duration :: Float  -- | Duration of the 'Win' game state.
    ,   timePassed :: Float  -- | Time passed in the YouWin state.
    ,   congrats :: Picture -- | Image rendered in the You Win state.
    }    
        deriving Show

-- |  Function creates and returns the menu state. System time is passed as a parameter.
getMenu :: Int -> GameState 
getMenu time = Menu 
            { currentTime = time
            , newGame = LivePicture.create (GlossGame.png ".\\assets\\newGame.png") 500 125 (0 , 50)  
            , quitGame = LivePicture.create (GlossGame.png ".\\assets\\quitGame.png") 500 125 (0 , -50) 
            }

-- | Funstion creates and returns the Play state. 
-- System time is passed as a paramter which is used for a random number generator and random distribution of cards.
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
                -- | create a random generator based on the system time.                                    
                randomGenerator = mkStdGen time
                -- | randomize coordinates with the given random generator.
                coordinates = shuffle' startCoordinates (length startCoordinates) randomGenerator                         

-- | Funstion creates and returns the GameOber state.
getGameOver :: GameState
getGameOver = GameOver
                    { duration = 3
                    , timePassed = 0
                    , endGame = GlossGame.png ".\\assets\\gameover.png"
                    }

-- | Funstion creates and returns the YouWin state.
getYouWin :: GameState
getYouWin = YouWin
            { duration = 3
            , timePassed = 0
            , congrats = GlossGame.png ".\\assets\\youwin3.png"
            }
                   