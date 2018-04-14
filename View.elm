module View exposing (..)

import Html exposing (button, div, h1, h3, text)
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
      [ h3 [] [ text title ]
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


playerString : Player -> String
playerString player =
  case player of
    PlayerOne ->
      "Player One"
    PlayerTwo ->
      "Player Two"


roundDisplay : Round -> Html.Html Update.Msg
roundDisplay round =
  div
    []
    [ text ("Dealer: " ++ playerString round.dealer)
    , scoreDisplay "Score" round.score
    , knockButtons
    , Html.hr [] []
    ]


view : Model -> Html.Html Update.Msg
view model =
  div
    []
    [ h1 [] [ text "Gin Score" ]
    , div [] (List.map roundDisplay model.rounds)
    , scoreDisplay "Sum of Rounds" model.roundTotal
    ]
