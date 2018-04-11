module View exposing (..)

import Update
import Model
import Html
import Html.Events
import Html.Attributes


view : Model.Model -> Html.Html Update.Msg
view model =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Gin Score" ]
        , Html.p [] [ Html.text "Yo" ]
        ]
