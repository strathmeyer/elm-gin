module Model.Round exposing (..)


import Model.Player exposing (Player)
import Model.Score exposing (Score)


type alias Round =
  { dealer : Player
  , knocker : Maybe Player
  , winner : Maybe Player
  , deadwood : Score
  , score : Score
  }
