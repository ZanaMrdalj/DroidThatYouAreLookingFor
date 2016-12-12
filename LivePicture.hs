module LivePicture (LivePicture(..), isClicked) where

import Graphics.Gloss

type Position = (Int, Int)

data LivePicture = LivePicture 
    { picture :: Picture
    , width :: Int
    , height :: Int
    , position :: Position
    } deriving (Show)

isClicked :: LivePicture -> (Float, Float) -> Bool
isClicked (LivePicture _ width height (posX, posY)) (mouseX, mouseY) = (mouseX > fromIntegral posX - fromIntegral (width `div` 2)) && 
                                                                        (mouseX < fromIntegral posX + fromIntegral (width `div` 2 )) && 
                                                                        (mouseY < fromIntegral posY + fromIntegral (height `div` 2)) &&
                                                                        (mouseY > fromIntegral posY - fromIntegral (height `div` 2))     