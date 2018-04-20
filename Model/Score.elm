module Model.Score exposing (..)


import Model.Player exposing (..)

type alias Score =
  { playerOne : Int
  , playerTwo : Int
  }


sum : List Score -> Score
sum scores =
  let
    playerTotal player =
      List.sum (List.map (playerScore player) scores)

  in
    Score
      (playerTotal PlayerOne)
      (playerTotal PlayerTwo)


playerScore : Player -> Score -> Int
playerScore player score =
  case player of
    PlayerOne ->
      score.playerOne
    PlayerTwo ->
      score.playerTwo
