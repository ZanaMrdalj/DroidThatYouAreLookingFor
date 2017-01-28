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
import Data.Time
import Data.Time.Clock.POSIX
import System.Random

render :: GameState -> Picture
render (Intro _ duration timePassed introPicture) = introPicture
render (Menu _ (LivePicture newGamePicture _ _ _) (LivePicture quitGamePicture _ _ _)) = pictures [newGamePicture, quitGamePicture]
render (Play _ _ duration timePassed cards matchingCards) = pictures pictureList
    where
        currentTime = ceiling (duration - timePassed) 
        timer = Timer.getTimer currentTime
        pictureList = timer : [ Card.getPicture card | card <- cards ]
render (GameOver duration timePassed exitPicture) = exitPicture
render (YouWin duration timePassed winningPicture) = winningPicture


handleKeyEvents :: Event -> GameState -> GameState
handleKeyEvents (EventKey  (MouseButton LeftButton) Up _ mousePosition ) gameState@(Menu currentTime newGamePicture quitGamePicture) = if LivePicture.isClicked newGamePicture mousePosition  
                                                                                                                            then
                                                                                                                                GameState.getPlay currentTime
                                                                                                                            else
                                                                                                                                if LivePicture.isClicked quitGamePicture mousePosition
                                                                                                                                    then
                                                                                                                                        error "Quit Game"
                                                                                                                                    else
                                                                                                                                        gameState


handleKeyEvents ( EventKey  (MouseButton LeftButton) Up _ mousePosition ) gameState@(Play _ clickNumber duration timePassed cards matchingCards) = gameState { clickNumber = clickNumber', cards = cards', matchingCards = matchingCards' }
    where
        clickedCard = getClickedCard cards

        cards' = if (not $ isClickDisabled $ matchingCards) && (isJust clickedCard) then map checkCardClick cards else cards

        clickNumber' = if (not $ isClickDisabled $ matchingCards) && (isJust clickedCard)
                        then clickNumber + 1
                        else clickNumber
                      
        matchingCards' = if (not $ isClickDisabled $ matchingCards) && (isJust clickedCard)
                          then if clickNumber' `mod` 2 /=  0 
                                    then ( clickedCard ,  Nothing)
                                    else ( fst matchingCards , clickedCard)                                   
                           else matchingCards                           

      
        checkCardClick :: Card -> Card  --funkcija koja pokrece animaciju ako je karta kliknuta
        checkCardClick card@(Card front back isFlipped isAnimating _ _ _) = if not isFlipped && not isAnimating && LivePicture.isClicked back mousePosition
                                                                        then
                                                                            Card.startFlipAnimation card
                                                                        else                               
                                                                            card
        getClickedCard :: [Card] -> Maybe Card  --funkcija koja izdvaja kliknutu kartu iz niza
        getClickedCard [] = Nothing
        getClickedCard (card@(Card front back isFlipped isAnimating _ _ _) : cards) = if not isFlipped && not isAnimating && LivePicture.isClicked back mousePosition
                                                                        then
                                                                            Just card
                                                                        else
                                                                            getClickedCard cards       
        isClickDisabled :: (Maybe Card, Maybe Card) -> Bool
        isClickDisabled (Nothing, Nothing) = False
        isClickDisabled (Just card, Nothing) = False
        isClickDisabled (Nothing, Just card) = False
        isClickDisabled (Just card1 , Just card2) = True

                                                                                                                             
handleKeyEvents _ gameState = gameState

updateGameState :: Float -> GameState -> GameState
updateGameState seconds gameState@(Intro currentTime duration timePassed introPicture) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if timePassed' < duration  --prikazujemo Intro stanje par sekundi i prelazimo u Meni stanje
                        then
                            gameState { timePassed = timePassed' }
                        else
                            GameState.getMenu currentTime

updateGameState seconds gameState@(Menu _ _ _)  = gameState

updateGameState seconds gameState@(Play _ clickNumber duration timePassed cards matchingCards) = gameState'
    where
        timePassed' = timePassed + seconds
        gameState' = if allCardsFliped cards'   
                        then GameState.getYouWin  --ako su sve karte okrenute (uparene), igra je zavrsena i prelazimo u stanje YouWin
                        else if timePassed' < duration 
                                then 
                                    gameState { timePassed = timePassed', cards = cards', matchingCards = matchingCards' }
                                else  -- vreme je isteklo, igra je zavrsena i prelazimo u stanje GameOver
                                    GameState.getGameOver      


        cards' = map updateCards cards

        matchingCards' = if isCardMatch matchingCards
                        then (Nothing, Nothing)
                        else if isCardMismatch matchingCards && mismatchAnimationComplete 
                            then (Nothing, Nothing)
                            else matchingCards                 
                            
        allCardsFliped :: [Card] -> Bool             
        allCardsFliped cards = all Card.isFlipped cards'

        isCardMismatch :: (Maybe Card, Maybe Card) -> Bool
        isCardMismatch (Nothing, Nothing) = False
        isCardMismatch (Just card, Nothing) = False
        isCardMismatch (Nothing, Just card) = False
        isCardMismatch (Just card1 , Just card2) = if Card.cardId card1 /= Card.cardId card2 then True else False

        isCardMatch :: (Maybe Card, Maybe Card) -> Bool
        isCardMatch (Nothing, Nothing) = False
        isCardMatch (Just card, Nothing) = False
        isCardMatch (Nothing, Just card) = False
        isCardMatch (Just card1 , Just card2) = if Card.cardId card1 == Card.cardId card2 then True else False

        mismatchAnimationComplete :: Bool
        mismatchAnimationComplete = allAnimationsComplete == 4 
            where
            allAnimationsComplete = foldl animationComplete 0 cards'
            
            animationComplete :: Int -> Card -> Int
            animationComplete acc card@(Card _ _ isFlipped isAnimating _ _ cardId) = if (cardId == fstCardId || cardId == sndCardId)  && not isFlipped &&  not isAnimating 
                                                                                        then acc + 1
                                                                                        else acc
                                                                                     where
                                                                                     fstCardId = Card.cardId ( fromJust (fst matchingCards))
                                                                                     sndCardId = Card.cardId ( fromJust (snd matchingCards))
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
                                                                                                                        sndCardId = Card.cardId ( fromJust (snd matchingCards))
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
    time <- round `fmap` getPOSIXTime  
    let initialGameState = Intro {
        currentTime = time 
        , duration = 2 
        , timePassed = 0
        , introPicture = GlossGame.jpg ".\\assets\\intro.jpg" } in 
        play GameWindow.window GameWindow.backgroundColor GameWindow.fps initialGameState render handleKeyEvents updateGameState
