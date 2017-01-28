module LivePicture (LivePicture(..), Position, create, isClicked) where  

import Graphics.Gloss

type Position = (Float, Float)

data LivePicture = LivePicture 
    { picture :: Picture
    , width :: Int
    , height :: Int
    , position :: Position
    } deriving (Show)

create :: Picture -> Int -> Int -> Position -> LivePicture   --funkcija koja od slike pravi povrsinu na koju je moguce kliknuti
create picture width height (x, y)  = LivePicture 
                                                 { picture = translate x y $ picture
                                                 , width = width
                                                 , height = height
                                                 , position = (x, y)
                                                 }

isClicked :: LivePicture -> (Float, Float) -> Bool   --funkcija koja proverava da li smo kliknuli na karticu
isClicked (LivePicture _ width height (posX, posY)) (mouseX, mouseY) = (mouseX > posX - fromIntegral (width `div` 2)) && 
                                                                        (mouseX < posX + fromIntegral (width `div` 2 )) && 
                                                                        (mouseY < posY + fromIntegral (height `div` 2)) &&
                                                                        (mouseY > posY - fromIntegral (height `div` 2))     