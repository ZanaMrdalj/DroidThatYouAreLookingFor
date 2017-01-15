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
render (Play _ duration timePassed cards ) = pictures pictureList
    where
        currentTime = ceiling (duration - timePassed) 
        timer = Timer.getTimer currentTime
        pictureList = timer : [ Card.getPicture card | card <- cards ]
render (GameOver duration timePassed exitPicture) = exitPicture
render (YouWin duration timePassed winningPicture) = winningPicture


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


handleKeyEvents ( EventKey  (MouseButton LeftButton) Up _ mousePosition ) gameState@(Play clickNumber duration timePassed cards) = gameState { clickNumber = clickNumber', cards = cards' }
    where
        cards' = map checkCardClick cards

        clickNumber' = if clickNumber == 2 then  0 
                                           else clickNumber + 1

        -- toPolje' = 
        --            fja :: [card] -> card
        --            fja cards                       

        checkCardClick :: Card -> Card
        checkCardClick card@(Card front back isFlipped isAnimating _ _ _) = if not isFlipped && not isAnimating && LivePicture.isClicked back mousePosition
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
updateGameState seconds gameState@(Play clickNumber duration timePassed cards) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if timePassed' < duration 
                        then 
                            gameState { timePassed = timePassed', cards = cards' }
                        else 
                            GameState.getGameOver      
                            
        cards' = map updateCards cards

        updateCards :: Card -> Card
        updateCards card@(Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) = if isAnimating
                                                                                                            then
                                                                                                                if animationTimePassed > animationDuration
                                                                                                                    then
                                                                                                                        Card.stopFlipAnimation card
                                                                                                                    else
                                                                                                                        Card.updateFlipAnimation seconds card
                                                                                                            else
                                       
                                                                                                                card
                                                                                                                
   --    checkCardsMatch :: Card -> Card -> GameState
   --    checkCardsMatch card1@(Card front back isFlipped _ _ _ cardId1) card2@(Card front back isFlipped _ _ _ cardId1)  
   --    | cardId1 != cardId2 = --treba flipovati karte nazad
   --    | cardId == 6 =   --treba preci u stanje youWin
   --
   --    |otherwise  =      --nista, ostaviti ih otvorene                                                                                                              
                                                                                                                                                                                                                            

updateGameState seconds gameState@(YouWin duration timePassed introPicture) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if timePassed' < duration
                        then
                            gameState { timePassed = timePassed' }
                        else
                            error "Thank you for playing"


updateGameState seconds gameState@(GameOver duration timePassed exitPicture) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if timePassed' < duration
                        then
                            gameState { timePassed = timePassed' }
                        else
                            error "Quit game"

main :: IO ()
main = do 
    let initialGameState = Intro { 
        duration = 2 
        , timePassed = 0
        , introPicture = GlossGame.jpg ".\\assets\\intro.jpg" } in 
        play GameWindow.window GameWindow.backgroundColor GameWindow.fps initialGameState render handleKeyEvents updateGameState
