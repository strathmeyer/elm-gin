module Update exposing (..)

import Model


type Msg
    = SetCurrentText String


update : Msg -> Model.Model -> Model.Model
update msg model =
    model
