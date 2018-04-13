module Update exposing (..)

import Model


type Msg
    = Knock Model.Player


update : Msg -> Model.Model -> Model.Model
update msg model =
    model
