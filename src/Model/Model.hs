module Model.Model where

initialState = MkGameState {
    test = 0,
    test1 = 0
}

data GameState = MkGameState {
    test        :: Int,
    test1       :: Int
}