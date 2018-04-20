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
      List.sum (List.map (get player) scores)

  in
    Score
      (playerTotal PlayerOne)
      (playerTotal PlayerTwo)


get : Player -> Score -> Int
get player score =
  case player of
    PlayerOne ->
      score.playerOne
    PlayerTwo ->
      score.playerTwo
