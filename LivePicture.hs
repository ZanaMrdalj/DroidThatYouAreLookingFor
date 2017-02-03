module LivePicture (LivePicture(..), Position, create, isClicked) where  

import Graphics.Gloss

type Position = (Float, Float)

-- | 'LivePicture' represents a clickable Gloss Picture. 
data LivePicture = LivePicture 
    { picture :: Picture -- | Gloss Picture
    , width :: Int 
    , height :: Int
    , position :: Position -- | Position of the picture on the screen
    } deriving (Show)

-- | 'create' constructs a LivePicture from the given parameters
create :: Picture -> Int -> Int -> Position -> LivePicture   
create picture width height (x, y)  = LivePicture 
                                                 { picture = translate x y $ picture
                                                 , width = width
                                                 , height = height
                                                 , position = (x, y)
                                                 }

-- | 'isClicked' checks whether a given LivePicture was clicked based on the passed mouse position coordinates.
isClicked :: LivePicture -> (Float, Float) -> Bool
isClicked (LivePicture _ width height (posX, posY)) (mouseX, mouseY) = (mouseX > posX - fromIntegral (width `div` 2)) && 
                                                                        (mouseX < posX + fromIntegral (width `div` 2 )) && 
                                                                        (mouseY < posY + fromIntegral (height `div` 2)) &&
                                                                        (mouseY > posY - fromIntegral (height `div` 2))     