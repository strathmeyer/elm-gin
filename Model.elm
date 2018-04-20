module Model
  exposing
    ( GameState(..)
    , Model
    , init
    , roundInit
    )


import Model.Player exposing ( Player(..) )
import Model.Round exposing ( Round )
import Model.Score exposing ( Score )


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


twoPlayerScoreInit =
  Score 0 0



roundInit : Player -> Round
roundInit dealer =
  Round
    dealer
    Nothing
    Nothing
    twoPlayerScoreInit
    twoPlayerScoreInit


init : Model
init =
  Model
    ""
    ""
    [ roundInit PlayerOne ]
    InProgress
    twoPlayerScoreInit
    twoPlayerScoreInit
    twoPlayerScoreInit
