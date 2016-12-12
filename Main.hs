module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Game as GlossGame
import GameState
import LivePicture

windowResolution :: (Int, Int) -- width and height of the window
windowResolution = (1280, 720)

windowPosition :: (Int, Int)
windowPosition = (10,10)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

window :: Display
window = InWindow "Droid That You Are Looking For" windowResolution windowPosition


render :: GameState -> Picture
render (Intro duration timePassed introPicture) = introPicture
render (Menu (LivePicture newGamePicture _ _ _) (LivePicture quitGamePicture _ _ _)) = pictures [newGamePicture, quitGamePicture]
render (Play duration timePassed ) = pictures [timer]
    where
        currentTime = ceiling (duration - timePassed)
        timer = scale 0.2 0.2 $ color white $ text $ show currentTime
        

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
updateGameState seconds gameState@(Play duration timePassed) = gameState {timePassed = timePassed'}
    where
        timePassed' = if timePassed + seconds < duration then timePassed + seconds else timePassed


main :: IO ()
main = do 
    let initialGameState = Intro { 
        duration = 2 
        , timePassed = 0
        , introPicture = GlossGame.jpg ".\\assets\\intro.jpg" } in 
        play window backgroundColor fps initialGameState render handleKeyEvents updateGameState
