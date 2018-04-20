module Model.Round
  exposing
    ( Round
    , addDeadwood
    , addKnocker
    , addWinner
    , boxScore
    , init
    )


import Model.Player as Player exposing (Player(..))
import Model.Score as Score exposing (Score)


type alias Round =
  { dealer : Player
  , knocker : Maybe Player
  , winner : Maybe Player
  , deadwood : Score
  , score : Score
  }


addDeadwood : Player -> Int -> Round -> Round
addDeadwood player newScore round =
  { round | deadwood = Score.update player newScore round.deadwood }


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
    { round
    | winner = Just winner
    , score = Score.update winner winnerScore Score.init
    }


roundWonByPlayer : Player -> Round -> Bool
roundWonByPlayer player round =
  case round.winner of
    Nothing ->
      False

    Just winner ->
      winner == player


winCount : Player -> List Round -> Int
winCount player rounds =
  List.length (List.filter (roundWonByPlayer player) rounds)


boxScore : List Round -> Score
boxScore rounds =
  Score
    ((winCount PlayerOne rounds) * 20)
    ((winCount PlayerTwo rounds) * 20)


init : Player -> Round
init dealer =
  Round
    dealer
    Nothing
    Nothing
    Score.init
    Score.init
