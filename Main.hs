module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Game as GlossGame
import GameWindow
import GameState
import LivePicture
import Timer
import Card
import Data.Maybe

render :: GameState -> Picture
render (Intro duration timePassed introPicture) = introPicture
render (Menu (LivePicture newGamePicture _ _ _) (LivePicture quitGamePicture _ _ _)) = pictures [newGamePicture, quitGamePicture]
render (Play _ _ duration timePassed cards matchingCards) = pictures pictureList
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


handleKeyEvents ( EventKey  (MouseButton LeftButton) Up _ mousePosition ) gameState@(Play forbidClick clickNumber duration timePassed cards matchingCards) = gameState { clickNumber = clickNumber', cards = cards', matchingCards = matchingCards' }
    where
        cards' = map checkCardClick cards

        clickNumber' = clickNumber + 1
                      
        matchingCards' = if clickNumber' `mod` 2 /=  0 
                                    then ( clickedCard ,  Nothing)
                                    else ( fst matchingCards , clickedCard) 
                                    where clickedCard = getClickedCard cards

      
        checkCardClick :: Card -> Card
        checkCardClick card@(Card front back isFlipped isAnimating _ _ _) = if not isFlipped && not isAnimating && LivePicture.isClicked back mousePosition
                                                                        then
                                                                            Card.startFlipAnimation card
                                                                        else                               
                                                                            card
        getClickedCard :: [Card] -> Maybe Card
        getClickedCard [] = Nothing
        getClickedCard (card@(Card front back isFlipped isAnimating _ _ _) : cards) = if not isFlipped && not isAnimating && LivePicture.isClicked back mousePosition
                                                                        then
                                                                            Just card
                                                                        else
                                                                            getClickedCard cards                                                                       

                                                                                                                             
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
updateGameState seconds gameState@(Play forbidClick clickNumber duration timePassed cards matchingCards) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if allCardsFliped cards' 
                        then GameState.getYouWin
                        else if timePassed' < duration 
                                then 
                                    gameState { timePassed = timePassed', cards = cards', matchingCards = matchingCards' }
                                else 
                                    GameState.getGameOver      


        cards' = map updateCards cards

        matchingCards' = if isCardMismatch matchingCards || isCardMatch matchingCards
                        then (Nothing, Nothing)
                        else matchingCards                 
                            
        allCardsFliped :: [Card] -> Bool             
        allCardsFliped cards = all Card.isFlipped cards'


        isCardMismatch :: (Maybe Card, Maybe Card) -> Bool
        isCardMismatch (Nothing, Nothing) = False
        isCardMismatch (Just card, Nothing) = False
        isCardMismatch (Nothing, Just card) = False
        isCardMismatch (Just card1 , Just card2) = if cardId card1 /= cardId card2 then True else False

        isCardMatch :: (Maybe Card, Maybe Card) -> Bool
        isCardMatch (Nothing, Nothing) = False
        isCardMatch (Just card, Nothing) = False
        isCardMatch (Nothing, Just card) = False
        isCardMatch (Just card1 , Just card2) = if cardId card1 == cardId card2 then True else False

        

        updateCards :: Card -> Card
        updateCards card@(Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) = if isAnimating
                                                                                                            then
                                                                                                                if animationTimePassed > animationDuration
                                                                                                                    then
                                                                                                                        Card.stopFlipAnimation card
                                                                                                                    else
                                                                                                                        Card.updateFlipAnimation seconds card
                                                                                                            else
                                                                                                                if isCardMismatch matchingCards
                                                                                                                then 
                                                                                                                    let fstCardId = Card.cardId ( fromJust (fst matchingCards))
                                                                                                                        sndCardId = Card.cardId $ fromJust $ snd matchingCards 
                                                                                                                    in if (cardId == fstCardId || cardId == sndCardId) && isFlipped
                                                                                                                        then
                                                                                                                         Card.startFlipAnimation card
                                                                                                                        else
                                                                                                                         card 
                                                                                                                else
                                                                                                                    card
                                                                                                                                                                                                                        
                                                                                                                                                                                                                            

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
