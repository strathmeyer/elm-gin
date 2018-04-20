module Model.Round exposing (..)


import Model.Player as Player exposing (Player(..))
import Model.Score as Score exposing (Score)


type alias Round =
  { dealer : Player
  , knocker : Maybe Player
  , winner : Maybe Player
  , deadwood : Score
  , score : Score
  }


addKnocker : Player -> Round -> Round
addKnocker player round =
  { round | knocker = Just player }


addWinner : Round -> Player -> Round
addWinner round knocker =
  let
    knockerDeadwood =
      Score.get knocker round.deadwood

    poneDeadwood =
      Score.get (Player.other knocker) round.deadwood

    gin = knockerDeadwood == 0

    winner =
      if gin || knockerDeadwood < poneDeadwood then
        knocker
      else
        Player.other knocker

    winnerScore =
      if knocker /= winner then
        (knockerDeadwood - poneDeadwood) + 10
      else
        if gin then
          poneDeadwood + 20
        else
          poneDeadwood - knockerDeadwood
  in
    { round |
        winner = Just winner,
        score = Score.update winner winnerScore Score.init
    }


init : Player -> Round
init dealer =
  Round
    dealer
    Nothing
    Nothing
    Score.init
    Score.init
