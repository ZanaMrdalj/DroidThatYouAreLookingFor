module GlobalState (GlobalState(..)) where


import GameState

data GlobalState = GlobalState {
            gameState :: GameState
        }