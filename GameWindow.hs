module GameWindow (
    windowResolution
    , windowPosition
    , backgroundColor
    , fps
    , window
    ) where

import Graphics.Gloss

windowResolution :: (Int, Int) -- sirina i visina prozora
windowResolution = (1280, 720)

windowPosition :: (Int, Int)
windowPosition = (10,10)

backgroundColor :: Color
backgroundColor = black

fps :: Int
fps = 60

window :: Display
window = InWindow "Droid That You Are Looking For" windowResolution windowPosition