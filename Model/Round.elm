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
    nonKnocker = Player.other knocker

    knockerScore =
      Score.get knocker round.deadwood

    poneScore =
      Score.get nonKnocker round.deadwood

    winner =
      if knockerScore > 0 && knockerScore >= poneScore then
        nonKnocker
      else
        knocker

    winnerScore =
      if winner == knocker then
        if knockerScore == 0 then
          -- gin
          poneScore + 20
        else
          -- normal score
          poneScore - knockerScore
      else
        -- knocker was undercut
        (knockerScore - poneScore) + 10

    score =
      case winner of
        PlayerOne ->
          Score winnerScore 0
        PlayerTwo ->
          Score 0 winnerScore
  in
    { round |
        winner = Just winner,
        score = score
    }
