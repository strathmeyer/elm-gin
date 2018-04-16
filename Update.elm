module Update exposing (..)

import Model exposing (Model, Player(..), Round, Score, playerScore)


type Msg
  = Knock Player
  | RoundScore Player String
  | SubmitRound


addKnocker : Player -> Round -> Round
addKnocker player round =
  { round | knocker = Just player }


updateScore : Player -> Int -> Round -> Round
updateScore player score round =
  let
    playerOneScore =
      playerScore PlayerOne round.score

    playerTwoScore =
      playerScore PlayerTwo round.score
  in
    { round | score = case player of
      PlayerOne ->
        [ Score PlayerOne score
        , Score PlayerTwo playerTwoScore
        ]
      PlayerTwo ->
        [ Score PlayerOne playerOneScore
        , Score PlayerTwo score
        ]
    }


update : Msg -> Model -> Model
update msg model =
  let
    round = List.head model.rounds
    tail = Maybe.withDefault [] (List.tail model.rounds)
  in
    case round of
      Nothing ->
        model
      Just r ->
        case msg of
          Knock player ->
            { model | rounds = (addKnocker player r) :: tail }
          RoundScore player string ->
            case String.toInt string of
              Err e ->
                model
              Ok score ->
                { model | rounds = (updateScore player score r) :: tail }
          SubmitRound ->
            model
