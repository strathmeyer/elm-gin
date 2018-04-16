module Update exposing (..)

import Model
  exposing
  ( GameState(..)
  , Model
  , Player(..)
  , Round
  , Score
  , playerScore
  , roundInit
  , sumScores
  )


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


addWinner : Round -> Player -> Round
addWinner round knocker =
  let
    nonKnocker =
      case knocker of
        PlayerOne -> PlayerTwo
        PlayerTwo -> PlayerOne

    knockerScore =
      playerScore knocker round.deadwood

    poneScore =
      playerScore nonKnocker round.deadwood

    winner =
      if knockerScore > 0 && knockerScore >= poneScore then
        nonKnocker
      else
        knocker

    loser =
      case winner of
        PlayerOne -> PlayerTwo
        PlayerTwo -> PlayerOne

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

  in
    { round |
        winner = Just winner,
        score =
          [ Score winner winnerScore
          , Score loser 0
          ]
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

boxScore : List Round -> List Score
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
    [ Score PlayerOne ((winCount PlayerOne rounds) * 20)
    , Score PlayerTwo ((winCount PlayerTwo rounds) * 20)
    ]


checkForGameEnd : Model -> Model
checkForGameEnd model =
  let
    maxScore =
      Maybe.withDefault
        0
        (List.maximum (List.map .score model.roundTotal))
  in
    if maxScore < 100 then
      model

    else
      let
        boxTotal =
          boxScore model.rounds

        total =
          sumScores [boxTotal, model.roundTotal]

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

              nextDealer =
                case r.dealer of
                  PlayerOne -> PlayerTwo
                  PlayerTwo -> PlayerOne

              rounds =
                (roundInit nextDealer) :: updatedRound :: tail

              roundTotal
                = sumScores (List.map .score rounds)

            in
              -- check on whether the game is over
              checkForGameEnd { model
                | rounds = rounds
                , roundTotal = roundTotal
              }
