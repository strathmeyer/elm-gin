module Model.Round exposing
    ( Round
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
        (winCount PlayerOne rounds * 20)
        (winCount PlayerTwo rounds * 20)


init : Player -> Round
init dealer =
    Round
        dealer
        Nothing
        Nothing
        Score.init
        Score.init
