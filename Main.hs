module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Game as GlossGame
import GameWindow
import GameState
import LivePicture
import Timer
import Card

render :: GameState -> Picture
render (Intro duration timePassed introPicture) = introPicture
render (Menu (LivePicture newGamePicture _ _ _) (LivePicture quitGamePicture _ _ _)) = pictures [newGamePicture, quitGamePicture]
render (Play duration timePassed cards ) = pictures pictureList
    where
        currentTime = ceiling (duration - timePassed)
        timer = Timer.getTimer currentTime
        pictureList = timer : [ Card.getPicture card | card <- cards ]
        

handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey  (MouseButton LeftButton) Up _ mousePosition ) gameState@(Menu newGamePicture quitGamePicture) = if LivePicture.isClicked newGamePicture mousePosition  
                                                                                                                            then
                                                                                                                                GameState.getPlay
                                                                                                                            else
                                                                                                                                if LivePicture.isClicked quitGamePicture mousePosition
                                                                                                                                    then
                                                                                                                                        error "Quit Game"
                                                                                                                                    else
                                                                                                                                        gameState

handleKeyEvents (EventKey  (MouseButton LeftButton) Up _ mousePosition ) gameState@(Play duration timePassed cards) = gameState { cards = cards' }
    where
        cards' = map checkCardClick cards

        checkCardClick :: Card -> Card
        checkCardClick card@(Card front back isFlipped isAnimating _ _ ) = if not isFlipped && not isAnimating && LivePicture.isClicked back mousePosition
                                                                        then
                                                                            Card.startFilpAnimation card
                                                                        else
                                                                            card
      
    
handleKeyEvents _ gameState = gameState

updateGameState :: Float -> GameState -> GameState
updateGameState seconds gameState@(Intro duration timePassed introPicture) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if timePassed' < duration
                        then
                            gameState { timePassed = timePassed' }
                        else
                            GameState.getMenu

updateGameState seconds gameState@(Menu _ _)  = gameState
updateGameState seconds gameState@(Play duration timePassed cards) = gameState {timePassed = timePassed', cards = cards'}
    where
        timePassed' = if timePassed + seconds < duration then timePassed + seconds else timePassed
        cards' = map updateCards cards

        updateCards :: Card -> Card
        updateCards card@(Card front back isFlipped isAnimating animationDuration animationTimePassed ) = if isAnimating
                                                                                                            then
                                                                                                                if animationTimePassed > animationDuration
                                                                                                                    then
                                                                                                                        Card.stopFlipAnimation card
                                                                                                                    else
                                                                                                                        Card.updateFlipAnimation seconds card
                                                                                                            else
                                                                                                                card

main :: IO ()
main = do 
    let initialGameState = Intro { 
        duration = 2 
        , timePassed = 0
        , introPicture = GlossGame.jpg ".\\assets\\intro.jpg" } in 
        play GameWindow.window GameWindow.backgroundColor GameWindow.fps initialGameState render handleKeyEvents updateGameState
