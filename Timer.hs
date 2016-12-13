module Timer (getTimer) where

import Graphics.Gloss
import GameWindow 

getTimer :: Int -> Picture
getTimer currentTime = translate x y $ scale 0.2 0.2 $ color white $ text $ show currentTime
    where
        x = - ( fromIntegral $ fst GameWindow.windowResolution) / 2 + 10
        y= - ( fromIntegral $ snd GameWindow.windowResolution) / 2 + 10
