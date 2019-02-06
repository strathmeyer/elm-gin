module Update exposing (Msg(..), addDeadwood, addKnocker, addWinner, checkForGameEnd, handleSubmit, update, updateLatestRound, updateRoundTotal)

import Model
    exposing
        ( GameState(..)
        , Model
        )
import Model.Player as Player exposing (Player(..))
import Model.Round as Round exposing (Round)
import Model.Score as Score exposing (Score)


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
                knocker
                round


addDeadwood : Player -> Int -> Round -> Round
addDeadwood player newScore round =
    { round | deadwood = Score.update player newScore round.deadwood }


addKnocker : Player -> Round -> Round
addKnocker player round =
    { round | knocker = Just player }


addWinner : Player -> Round -> Round
addWinner knocker round =
    let
        knockerDeadwood =
            Score.get knocker round.deadwood

        poneDeadwood =
            Score.get (Player.other knocker) round.deadwood

        gin =
            knockerDeadwood == 0

        winner =
            if gin || knockerDeadwood < poneDeadwood then
                knocker

            else
                Player.other knocker

        winnerScore =
            if knocker /= winner then
                (knockerDeadwood - poneDeadwood) + 10

            else if gin then
                poneDeadwood + 20

            else
                poneDeadwood - knockerDeadwood
    in
    { round
        | winner = Just winner
        , score = Score.update winner winnerScore Score.init
    }


checkForGameEnd : Model -> Model
checkForGameEnd model =
    if Score.max model.roundTotal < 100 then
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


updateLatestRound : (Round -> Round) -> Model -> Model
updateLatestRound updater model =
    let
        round =
            List.head model.rounds

        tail =
            Maybe.withDefault [] (List.tail model.rounds)
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
        round =
            List.head model.rounds

        tail =
            Maybe.withDefault [] (List.tail model.rounds)
    in
    case msg of
        Knock player ->
            updateLatestRound (addKnocker player) model

        Deadwood player string ->
            case String.toInt string of
                Nothing ->
                    model

                Just score ->
                    updateLatestRound (addDeadwood player score) model

        SubmitRound ->
            updateLatestRound handleSubmit model
                |> updateRoundTotal
                |> checkForGameEnd
