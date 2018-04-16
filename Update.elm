module Update exposing (..)

import Model exposing (Model, Player(..), Round, Score, playerScore)


type Msg
  = Knock Player
  | Deadwood Player String
  | SubmitRound


addKnocker : Player -> Round -> Round
addKnocker player round =
  { round | knocker = Just player }


updateScore : Player -> Int -> List Score -> List Score
updateScore player newScore scoreList =
  let
    playerOneScore =
      playerScore PlayerOne scoreList

    playerTwoScore =
      playerScore PlayerTwo scoreList
  in
    case player of
      PlayerOne ->
        [ Score PlayerOne newScore
        , Score PlayerTwo playerTwoScore
        ]
      PlayerTwo ->
        [ Score PlayerOne playerOneScore
        , Score PlayerTwo newScore
        ]


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
          Deadwood player string ->
            case String.toInt string of
              Err e ->
                model
              Ok score ->
                let
                  updatedRound = { r | deadwood = (updateScore player score r.deadwood) }
                in
                  { model | rounds = updatedRound :: tail }
          SubmitRound ->
            model
