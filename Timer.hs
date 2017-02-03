module Timer (getTimer) where

import Graphics.Gloss
import GameWindow 

-- | 'getTimer' function returns currentTime as a Gloss Picture positioned properly.
getTimer :: Int -> Picture  
getTimer currentTime = translate x y $ scale 0.2 0.2 $ color white $ text $ splitTimer currentTime
    where
        x = - ( fromIntegral $ fst GameWindow.windowResolution) / 2 + 630
        y= - ( fromIntegral $ snd GameWindow.windowResolution) / 2 + 10

-- | 'spliTimer' transformes given time in seconds to a formated string in form of min:sec.
splitTimer :: Int -> String   
splitTimer currentTime = show minutes ++ " : " ++ show seconds
      where minutes = currentTime `div` 60
            seconds = currentTime `mod` 60

