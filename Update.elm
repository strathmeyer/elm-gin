module Update exposing (..)

import Model exposing (Model, Player, Round)


type Msg
    = Knock Player


addKnocker : Player -> Round -> Round
addKnocker player round =
  { round | knocker = Just player }


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
