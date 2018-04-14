module View exposing (..)

import Html exposing (button, div, h1, text)
import Html.Events exposing (onClick)
import Update

import Model
  exposing
    ( Model
    , Player(..)
    , Round
    , Score
    , playerScore
    )


scoreDisplay : String -> List Score -> Html.Html Update.Msg
scoreDisplay title scoreList =
  let
    playerOneScore =
      toString (playerScore PlayerOne scoreList)
    playerTwoScore =
      toString (playerScore PlayerTwo scoreList)
  in
    div
      []
      [ h1 [] [ text title ]
      , div
        []
        [ text ("Player One: " ++ playerOneScore) ]
      , div
        []
        [ text ("Player Two: " ++ playerTwoScore) ]
      ]

knockButtons : Html.Html Update.Msg
knockButtons =
  div
    []
    [ button
      [ onClick (Update.Knock PlayerOne) ]
      [ text "Player One Knocks" ]
    , button
      [ onClick (Update.Knock PlayerTwo) ]
      [ text "Player Two Knocks" ]
    ]


view : Model -> Html.Html Update.Msg
view model =
  div
    []
    [ h1 [] [ text "Gin Score" ]
    , knockButtons
    , scoreDisplay "Sum of Rounds" model.roundTotal
    ]
