module Timer (getTimer) where

import Graphics.Gloss
import GameWindow 

getTimer :: Int -> Picture
getTimer currentTime = translate x y $ scale 0.2 0.2 $ color white $ text $ splitTimer currentTime
    where
        x = - ( fromIntegral $ fst GameWindow.windowResolution) / 2 + 630
        y= - ( fromIntegral $ snd GameWindow.windowResolution) / 2 + 10


splitTimer :: Int -> String
splitTimer currentTime
    | minutes >= 3 = "3 : " ++ show seconds
    | minutes >= 2 = "2 : " ++ show seconds
    | minutes >= 1 = "1 : " ++ show seconds
    | otherwise  = "0 : " ++ show seconds
    where minutes = currentTime `div` 60
          seconds = currentTime `mod` 60

