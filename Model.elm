module Model
  exposing
    ( GameState(..)
    , Model
    , init
    , lastDealer
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


lastDealer : Model -> Player
lastDealer model =
  case List.head model.rounds of
    Nothing ->
      PlayerTwo

    Just round ->
      round.dealer


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
