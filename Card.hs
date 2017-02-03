module Card (
    Card(..)
    , createCard
    , getPicture
    , startFlipAnimation
    , updateFlipAnimation
    , stopFlipAnimation
    ) where

import LivePicture
import Graphics.Gloss

-- | Data which represents one Card that is clickable and can be rendered on the screen.
data Card = Card 
    { front :: LivePicture -- | front face of the card
    , back :: LivePicture -- | back face of the card
    , isFlipped :: Bool  -- | flag which indicates whether the cards is flipped face up.
    , isAnimating :: Bool -- | flag that indicats if the card is currently animating (flipping from face to another)
    , animationDuration :: Float -- | duration of the flip animation
    , animationTimePassed :: Float -- | flip animation time passed
    , cardId :: Int  -- | card id used for differentiating between cards.
    } deriving (Show)  

-- | Default duration of the flip animation.
defaultAnimationDuration :: Float
defaultAnimationDuration = 0.8

-- | creates and returns a card based on given arguments.
createCard :: Picture -> Picture -> Int -> Int -> Position -> Int -> Card          
createCard frontPicture backPicture width height (x, y) matchNumber = Card 
                                                            { front = LivePicture.create frontPicture width height (x ,y)
                                                            , back = LivePicture.create backPicture width height (x, y)
                                                            , isFlipped = False
                                                            , isAnimating = False
                                                            , animationDuration = defaultAnimationDuration
                                                            , animationTimePassed = 0
                                                            , cardId = matchNumber   
                                                            } 
-- | gets a renderable Gloss picture of a given card.
getPicture :: Card -> Picture
getPicture card@(Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) = if not isFlipped 
                                                                                                then 
                                                                                                    if not isAnimating 
                                                                                                        then
                                                                                                            picture back
                                                                                                        else
                                                                                                            reverseFlipPicture card
                                                                                                else 
                                                                                                    if not isAnimating
                                                                                                        then
                                                                                                            picture front
                                                                                                        else
                                                                                                            flipPicture card
-- | scales the image based on given scale factors in the coordinate system orgin and returns a scaled version of renderable picture.
-- | used for flip animation                                                                                                             
scaleAroundOrigin :: Position -> Float -> Float -> Picture -> Picture
scaleAroundOrigin (x, y) scaleFactorX scaleFactorY picture =  (translate x y . scale scaleFactorX scaleFactorY . translate  (-x) (-y)) $ picture 

-- | Excutes flip animation (from back to front) based on the current Card state and its animation state.
flipPicture :: Card -> Picture
flipPicture (Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) 
    | animationTimePassed < treshold = let scaleX = (treshold - animationTimePassed) / treshold in scaleAroundOrigin (position back) scaleX 1 (picture back)
    | animationTimePassed > treshold && animationTimePassed < animationDuration = let scaleX = (animationTimePassed - treshold) / treshold in scaleAroundOrigin (position front) scaleX 1 (picture front)
    | otherwise = picture front
    where 
        treshold = animationDuration / 2

-- | Excutes flip animation (from front to back) based on the current Card state and its animation state.
reverseFlipPicture :: Card -> Picture
reverseFlipPicture (Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) 
    | animationTimePassed < treshold = let scaleX = (treshold - animationTimePassed) / treshold in scaleAroundOrigin (position front) scaleX 1  (picture front)
    | animationTimePassed > treshold && animationTimePassed < animationDuration = let scaleX = (animationTimePassed - treshold) / treshold in scaleAroundOrigin (position back) scaleX 1  (picture back)
    | otherwise = picture back
    where treshold = animationDuration / 2
                                                                                        

-- | Used fo starting the flip animation.
startFlipAnimation :: Card -> Card
startFlipAnimation card@(Card front back isFlipped _ _ _ _) = card { isFlipped = isFlipped', isAnimating = True}
    where
        isFlipped' = not isFlipped

-- | Used for updating current flip animation state.
updateFlipAnimation :: Float -> Card -> Card
updateFlipAnimation seconds card = card { animationTimePassed = animationTimePassed' }
    where
        animationTimePassed' = animationTimePassed card + seconds 

-- | Used for stoping flip animation.
stopFlipAnimation :: Card -> Card
stopFlipAnimation card = card { isAnimating = False, animationTimePassed = 0 }
