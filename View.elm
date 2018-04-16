module View exposing (..)

import Html exposing (button, div, h1, h3, input, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
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


hasPlayer : Maybe Player -> Bool
hasPlayer player =
  case player of
    Nothing ->
      False
    Just _ ->
      True


scoreInput : Html.Html Update.Msg
scoreInput =
  div
    []
    [ Html.label
      []
      [ text "Player One:"
      , input
          [ placeholder "Player One"
          , type_ "number"
          , onInput (Update.RoundScore Model.PlayerOne)
          ]
          []
      ]
    , Html.label
      []
      [ text "Player Two:"
      , input
          [ placeholder "Player Two"
          , type_ "number"
          , onInput (Update.RoundScore Model.PlayerTwo)
          ]
          []
      ]
    , input
        [ type_ "submit"
        , onClick Update.SubmitRound ]
        []
    ]

roundStateUI : Round -> Html.Html Update.Msg
roundStateUI round =
  if not (hasPlayer round.knocker) then
    knockButtons
  else if not (hasPlayer round.winner) then
    scoreInput
  else
    scoreDisplay "Score" round.score


roundDisplay : Round -> Html.Html Update.Msg
roundDisplay round =
  div
    []
    [ text ("Dealer: " ++ playerString round.dealer)
    , roundStateUI round
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
