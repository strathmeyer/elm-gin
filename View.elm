module View exposing (..)

import Update
import Model
import Html
import Html.Events
import Html.Attributes


--scoreDisplay : String -> Model.


view : Model.Model -> Html.Html Update.Msg
view model =
  Html.div
    []
    [ Html.h1 [] [ Html.text "Gin Score" ]
    , Html.p
      []
      [ Html.button
        [ Html.Events.onClick ( Update.Knock Model.PlayerOne ) ]
        [ Html.text "Player One Knocks" ]
      , Html.button
        [ Html.Events.onClick ( Update.Knock Model.PlayerTwo ) ]
        [ Html.text "Player Two Knocks" ]
      ]
    ]
