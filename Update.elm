module Update exposing (..)

import Model
  exposing
  ( GameState(..)
  , Model
  )

import Model.Player as Player exposing ( Player(..) )
import Model.Round as Round exposing ( Round, addDeadwood, addKnocker, addWinner )
import Model.Score as Score exposing ( Score )


type Msg
  = Knock Player
  | Deadwood Player String
  | SubmitRound


handleSubmit : Round -> Round
handleSubmit round =
  case round.knocker of
    Nothing ->
      round

    Just knocker ->
      addWinner
        round
        knocker


checkForGameEnd : Model -> Model
checkForGameEnd model =
  if (Score.max model.roundTotal) < 100 then
    Model.initNextRound model
  else
    let
      boxTotal =
        Round.boxScore model.rounds

      total =
        Score.sum
          [ boxTotal
          , Score.addBonus model.roundTotal
          ]
    in
      { model
      | state = Completed
      , boxTotal = boxTotal
      , total = total
      }


updateLatestRound : Model -> (Round -> Round) -> Model
updateLatestRound model updater =
  let
    round = List.head model.rounds
    tail = Maybe.withDefault [] (List.tail model.rounds)
  in
    case round of
      Nothing ->
        model

      Just r ->
        { model | rounds = updater r :: tail }


updateRoundTotal : Model -> Model
updateRoundTotal model =
  { model
  | roundTotal = Score.sum (List.map .score model.rounds)
  }

update : Msg -> Model -> Model
update msg model =
  let
    round = List.head model.rounds
    tail = Maybe.withDefault [] (List.tail model.rounds)
  in
    case msg of
      Knock player ->
        updateLatestRound model (addKnocker player)

      Deadwood player string ->
        case String.toInt string of
          Err e ->
            model
          Ok score ->
            updateLatestRound model (addDeadwood player score)

      SubmitRound ->
        updateLatestRound model handleSubmit
        |> updateRoundTotal
        |> checkForGameEnd
