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


update : Player -> Int -> Score -> Score
update player newScore score =
  case player of
    PlayerOne ->
      Score newScore score.playerTwo

    PlayerTwo ->
      Score score.playerOne newScore

init =
  Score 0 0
