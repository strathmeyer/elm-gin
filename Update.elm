module Update exposing (..)

import Model
  exposing
  ( GameState(..)
  , Model
  )

import Model.Player as Player exposing ( Player(..) )
import Model.Round as Round exposing ( Round, addKnocker, addWinner )
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


boxScore : List Round -> Score
boxScore rounds =
  let
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
  in
    Score
      ((winCount PlayerOne rounds) * 20)
      ((winCount PlayerTwo rounds) * 20)


winnerBonus : Score -> Score
winnerBonus roundTotal =
  let
    bonus =
      if (Score.min roundTotal) == 0 then
        200
      else
        100
  in
    if roundTotal.playerOne > roundTotal.playerTwo then
      Score bonus 0
    else
      Score 0 bonus


checkForGameEnd : Model -> Model
checkForGameEnd model =
  if (Score.max model.roundTotal) < 100 then
    model
  else
    let
      boxTotal =
        boxScore model.rounds

      total =
        Score.sum
          [ boxTotal
          , model.roundTotal
          , winnerBonus model.roundTotal
          ]
    in
      { model
      | state = Completed
      , boxTotal = boxTotal
      , total = total
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

          Deadwood player string ->
            case String.toInt string of
              Err e ->
                model
              Ok score ->
                let
                  deadwood =
                    Score.update player score r.deadwood

                  updatedRound =
                    { r | deadwood = deadwood }
                in
                  { model | rounds = updatedRound :: tail }

          SubmitRound ->
            let
              updatedRound =
                handleSubmit r

              nextDealer = Player.other r.dealer

              rounds =
                (Round.init nextDealer) :: updatedRound :: tail

              roundTotal
                = Score.sum (List.map .score rounds)
            in
              checkForGameEnd { model
                | rounds = rounds
                , roundTotal = roundTotal
              }
