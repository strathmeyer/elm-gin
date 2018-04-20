module Model
  exposing
    ( GameState(..)
    , Model
    , init
    )


import Model.Player as Player exposing ( Player(..) )
import Model.Round as Round exposing ( Round )
import Model.Score as Score exposing ( Score )


type GameState = InProgress | Completed


-- TODO: use player names
type alias Model =
    { playerOneName : String
    , playerTwoName : String
    , rounds : List Round
    , state : GameState
    , roundTotal : Score
    , boxTotal : Score
    , total : Score
    }


init : Model
init =
  Model
    ""
    ""
    [ Round.init PlayerOne ]
    InProgress
    Score.init
    Score.init
    Score.init
