module Model.Player exposing (..)


type Player = PlayerOne | PlayerTwo


other : Player -> Player
other player =
  case player of
    PlayerOne ->
      PlayerTwo
    PlayerTwo ->
      PlayerOne
