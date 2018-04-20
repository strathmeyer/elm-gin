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


max : Score -> Int
max score =
  Basics.max
    score.playerOne
    score.playerTwo


min : Score -> Int
min score =
  Basics.min
    score.playerOne
    score.playerTwo


init =
  Score 0 0
