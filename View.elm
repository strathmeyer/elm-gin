module View exposing (..)

import Html exposing (button, div, h1, h3, input, p, text)
import Html.Attributes exposing (placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Update

import Model
  exposing
    ( GameState(..)
    , Model
    , Player(..)
    , Round
    , Score
    , playerScore
    )


scoreDisplay : String -> Score -> Html.Html Update.Msg
scoreDisplay title score =
  let
    playerOneScore =
      toString (playerScore PlayerOne score)

    playerTwoScore =
      toString (playerScore PlayerTwo score)

  in
    div
      []
      [ Html.strong [] [ text title ]
      , text (" - "
            ++ playerString PlayerOne
            ++ ": "
            ++ playerOneScore
            ++ ", "
            ++ playerString PlayerTwo
            ++ ": "
            ++ playerTwoScore)
      ]


knockButtons : Html.Html Update.Msg
knockButtons =
  div
    []
    [ button
      [ onClick (Update.Knock PlayerOne) ]
      [ text (playerString PlayerOne ++ " Knocks") ]
    , button
      [ onClick (Update.Knock PlayerTwo) ]
      [ text (playerString PlayerTwo ++ " Knocks") ]
    ]


playerString : Player -> String
playerString player =
  case player of
    PlayerOne ->
      "A"
    PlayerTwo ->
      "B"


hasPlayer : Maybe Player -> Bool
hasPlayer player =
  case player of
    Nothing ->
      False
    Just _ ->
      True


deadwoodInput : Html.Html Update.Msg
deadwoodInput =
  div
    []
    [ Html.label
      []
      [ text (playerString PlayerOne ++ ":" )
      , input
          [ placeholder "Score"
          , type_ "number"
          , onInput (Update.Deadwood Model.PlayerOne)
          ]
          []
      ]
    , Html.label
      []
      [ text (playerString PlayerTwo ++ ":" )
      , input
          [ placeholder "Score"
          , type_ "number"
          , onInput (Update.Deadwood Model.PlayerTwo)
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
    deadwoodInput
  else
    div
      []
      [ scoreDisplay "Deadwood" round.deadwood
      , scoreDisplay "Score" round.score
      ]


roundDisplay : Int -> Round -> Html.Html Update.Msg
roundDisplay index round =
  div
    []
    [ p
        []
        [ text ("Round " ++
            toString (index + 1) ++
            " - " ++
            "Dealer: " ++
            playerString round.dealer)
        ]
    , roundStateUI round
    , Html.hr [] []
    ]

gameScore : Model -> List (Html.Html Update.Msg)
gameScore model =
  let
    basicDisplay = [ scoreDisplay "Sum of Rounds" model.roundTotal ]
  in
    case model.state of
      InProgress ->
        basicDisplay
      Completed ->
        basicDisplay ++
          [ scoreDisplay "Box Score" model.boxTotal
          , scoreDisplay "Total score" model.total
          ]


view : Model -> Html.Html Update.Msg
view model =
  let
    rounds = case model.state of
      InProgress ->
        model.rounds
      Completed ->
        Maybe.withDefault [] (List.tail model.rounds)
  in
    div
      []
      ([ h1 [] [ text "Gin Score" ]
      , div [] (List.indexedMap roundDisplay (List.reverse rounds))
      ] ++ gameScore model)
