module Model
  exposing
    ( GameState(..)
    , Model
    , init
    , playerScore
    , roundInit
    , sumScores
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


sumScores : List Score -> Score
sumScores scores =
  let
    playerTotal player =
      List.sum (List.map (playerScore player) scores)

  in
    Score
      (playerTotal PlayerOne)
      (playerTotal PlayerTwo)


twoPlayerScoreInit =
  Score 0 0


playerScore : Player -> Score -> Int
playerScore player score =
  case player of
    PlayerOne ->
      score.playerOne
    PlayerTwo ->
      score.playerTwo


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
