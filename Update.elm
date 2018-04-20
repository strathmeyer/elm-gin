module Update exposing (..)

import Model
  exposing
  ( GameState(..)
  , Model
  , roundInit
  )

import Model.Player as Player exposing ( Player(..) )
import Model.Round as Round exposing ( Round )
import Model.Score as Score exposing ( Score )


type Msg
  = Knock Player
  | Deadwood Player String
  | SubmitRound


addKnocker : Player -> Round -> Round
addKnocker player round =
  { round | knocker = Just player }


updateScore : Player -> Int -> Score -> Score
updateScore player newScore score =
  case player of
    PlayerOne ->
      Score newScore score.playerTwo
    PlayerTwo ->
      Score score.playerOne newScore


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
    winner = if roundTotal.playerOne > roundTotal.playerTwo then
      PlayerOne
    else
      PlayerTwo


    bonus =
      if min roundTotal.playerOne roundTotal.playerTwo == 0 then
        200
      else
        100


  in
    case winner of
      PlayerOne ->
        Score bonus 0

      PlayerTwo ->
        Score 0 bonus


checkForGameEnd : Model -> Model
checkForGameEnd model =
  let
    maxScore =
      max
        model.roundTotal.playerOne
        model.roundTotal.playerTwo

  in
    if maxScore < 100 then
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
                    updateScore player score r.deadwood

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
                (roundInit nextDealer) :: updatedRound :: tail

              roundTotal
                = Score.sum (List.map .score rounds)

            in
              checkForGameEnd { model
                | rounds = rounds
                , roundTotal = roundTotal
              }
