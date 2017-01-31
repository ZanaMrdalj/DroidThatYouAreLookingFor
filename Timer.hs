module Timer (getTimer) where

import Graphics.Gloss
import GameWindow 

getTimer :: Int -> Picture   --funkcija koja dohvata preostalo vreme i ispisuje ga 
getTimer currentTime = translate x y $ scale 0.2 0.2 $ color white $ text $ splitTimer currentTime
    where
        x = - ( fromIntegral $ fst GameWindow.windowResolution) / 2 + 630
        y= - ( fromIntegral $ snd GameWindow.windowResolution) / 2 + 10


splitTimer :: Int -> String   --funkcija koja formatira vreme u obliku min:sec
splitTimer currentTime = show minutes ++ " : " ++ show seconds
      where minutes = currentTime `div` 60
            seconds = currentTime `mod` 60

