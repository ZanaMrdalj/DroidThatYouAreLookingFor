module Card (
    Card(..)
    , createCard
    , getPicture
    , startFilpAnimation
    , updateFlipAnimation
    , stopFlipAnimation
    ) where

import LivePicture
import Graphics.Gloss

data Card = Card 
    { front :: LivePicture
    , back :: LivePicture
    , isFlipped :: Bool
    , isAnimating :: Bool
    , animationDuration :: Float
    , animationTimePassed :: Float
    , cardId :: Int
    } deriving (Show)  

defaultAnimationDuration :: Float
defaultAnimationDuration = 1.0

createCard :: Picture -> Picture -> Int -> Int -> Position -> Card          
createCard frontPicture backPicture width height (x, y)  = Card 
                                                            { front = LivePicture.create frontPicture width height (x ,y)
                                                            , back = LivePicture.create backPicture width height (x, y)
                                                            , isFlipped = False
                                                            , isAnimating = False
                                                            , animationDuration = defaultAnimationDuration
                                                            , animationTimePassed = 0
                                                            , cardId = 0   
                                                            } 

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
scaleAroundOrigin :: Position -> Float -> Float -> Picture -> Picture
scaleAroundOrigin (x, y) scaleFactorX scaleFactorY picture =  (translate x y . scale scaleFactorX scaleFactorY . translate  (-x) (-y)) $ picture 

flipPicture :: Card -> Picture
flipPicture (Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) 
    | animationTimePassed < treshold = let scaleX = (treshold - animationTimePassed) / treshold in scaleAroundOrigin (position back) scaleX 1 (picture back)
    | animationTimePassed > treshold && animationTimePassed < animationDuration = let scaleX = (animationTimePassed - treshold) / treshold in scaleAroundOrigin (position front) scaleX 1 (picture front)
    | otherwise = picture front
    where 
        treshold = animationDuration / 2

reverseFlipPicture :: Card -> Picture
reverseFlipPicture (Card front back isFlipped isAnimating animationDuration animationTimePassed cardId) 
    | animationTimePassed < treshold = let scaleX = (treshold - animationTimePassed) / treshold in scaleAroundOrigin (position front) scaleX 1  (picture front)
    | animationTimePassed > treshold && animationTimePassed < animationDuration = let scaleX = (animationTimePassed - treshold) / treshold in scaleAroundOrigin (position back) scaleX 1  (picture back)
    | otherwise = picture back
    where treshold = animationDuration / 2
                                                                                        


startFilpAnimation :: Card -> Card
startFilpAnimation card@(Card front back isFlipped _ _ _ _) = card { isFlipped = isFlipped', isAnimating = True}
    where
        isFlipped' = not isFlipped

updateFlipAnimation :: Float -> Card -> Card
updateFlipAnimation seconds card = card { animationTimePassed = animationTimePassed' }
    where
        animationTimePassed' = animationTimePassed card + seconds 

stopFlipAnimation :: Card -> Card
stopFlipAnimation card = card { isAnimating = False, animationTimePassed = 0 }
