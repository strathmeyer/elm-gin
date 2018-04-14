module View exposing (..)

import Update
import Model
import Html
import Html.Events
import Html.Attributes


scoreDisplay : String -> List Model.Score -> Html.Html Update.Msg
scoreDisplay title scoreList =
  let
    playerOneScore =
      toString (Model.playerScore Model.PlayerOne scoreList)
    playerTwoScore =
      toString (Model.playerScore Model.PlayerTwo scoreList)
  in
    Html.div
      []
      [ Html.h1 [] [ Html.text title ]
      , Html.div
        []
        [ Html.text ("Player One: " ++ playerOneScore) ]
      , Html.div
        []
        [ Html.text ("Player Two: " ++ playerTwoScore) ]
      ]


view : Model.Model -> Html.Html Update.Msg
view model =
  Html.div
    []
    [ Html.h1 [] [ Html.text "Gin Score" ]
    , Html.div
      []
      [ Html.button
        [ Html.Events.onClick (Update.Knock Model.PlayerOne) ]
        [ Html.text "Player One Knocks" ]
      , Html.button
        [ Html.Events.onClick (Update.Knock Model.PlayerTwo) ]
        [ Html.text "Player Two Knocks" ]
      ]
    , scoreDisplay "Sum of Rounds" model.roundTotal
    ]
