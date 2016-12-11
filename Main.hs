module Main where

import GlobalState
import GameState

initialGlobalState :: GlobalState
initialGlobalState = GlobalState {
                        gameState = Intro
                    }

main :: IO ()
main = putStrLn "Hello, Haskell!"
