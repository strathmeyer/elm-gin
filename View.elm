module View exposing (..)

import Html exposing (button, div, h1, h3, input, p, text)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onClick, onInput)
import Update

import Model
  exposing
    ( GameState(..)
    , Model
    )


import Model.Player exposing ( Player(..) )
import Model.Round exposing ( Round )
import Model.Score exposing ( Score )


scoreDisplay : String -> Score -> Html.Html Update.Msg
scoreDisplay title score =
  div
    [ class "row" ]
    [ div
      [ class "col text-truncate" ]
      [ Html.strong [] [ text title ] ]
    , div
      [ class "col" ]
      [ text (playerString PlayerOne
        ++ ": "
        ++ toString score.playerOne) ]
    , div
      [ class "col" ]
      [ text (playerString PlayerTwo
        ++ ": "
        ++ toString score.playerTwo) ]
    ]


knockButtons : Html.Html Update.Msg
knockButtons =
  div
    [ class "row" ]
    [ div
      [ class "col" ]
      [ button
        [ onClick (Update.Knock PlayerOne)
        , class "btn btn-primary btn-block"
        , type_ "button"
        ]
        [ text (playerString PlayerOne ++ " Knocks") ]
      ]
    , div
      [ class "col" ]
      [ button
        [ onClick (Update.Knock PlayerTwo)
        , class "btn btn-primary btn-block"
        , type_ "button"
        ]
        [ text (playerString PlayerTwo ++ " Knocks") ]
      ]
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


--<label for="exampleInputEmail1">Email address</label>
--    <input type="email" class="form-control" id="exampleInputEmail1" aria-describedby="emailHelp" placeholder="Enter email">
deadwoodInput : Html.Html Update.Msg
deadwoodInput =
  div
    [ class "row" ]
    [ div
      [ class "col" ]
      [ input
        [ placeholder (playerString PlayerOne)
        , class "form-control"
        , type_ "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "100"
        , onInput (Update.Deadwood PlayerOne)
        ]
        []
      ]
    , div
      [ class "col" ]
      [ input
        [ placeholder (playerString PlayerTwo)
        , class "form-control"
        , type_ "number"
        , Html.Attributes.min "0"
        , Html.Attributes.max "100"
        , onInput (Update.Deadwood PlayerTwo)
        ]
        []
      ]
    , div
      [ class "col" ]
      [ button
        [ type_ "button"
        , class "btn btn-block btn-primary"
        , onClick Update.SubmitRound
        ]
        [ text "Submit" ]
      ]
    ]


roundStateUI : Round -> Html.Html Update.Msg
roundStateUI round =
  if not (hasPlayer round.knocker) then
    knockButtons
  else if not (hasPlayer round.winner) then
    deadwoodInput
  else
    div
      [ class "row" ]
      [ div
        [ class "col" ]
        [ scoreDisplay "Deadwood" round.deadwood
        , scoreDisplay "Score" round.score
        ]
      ]


roundDisplay : Int -> Round -> Html.Html Update.Msg
roundDisplay index round =
  div
    [ class "row pb-4 mb-4 border-bottom" ]
    [ div
      [ class "col" ]
      [ div
        [ class "row" ]
        [ p
          [ class "col" ]
          [ text ("Round " ++ toString (index + 1))]
        , p
          [ class "col" ]
          [ text ("Dealer: " ++ playerString round.dealer)]
        ]
      , roundStateUI round
      ]
    ]

gameScore : Model -> List (Html.Html Update.Msg)
gameScore model =
  let
    basicDisplay = [ scoreDisplay "Total" model.roundTotal ]
  in
    case model.state of
      InProgress ->
        basicDisplay
      Completed ->
        basicDisplay ++
          [ scoreDisplay "Box Score" model.boxTotal
          , scoreDisplay "Final" model.total
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
      [ class "container" ]
      ([ div
        [ class "row" ]
        [ div
          [ class "col" ]
          [ h1 [] [ text "Gin Score" ] ]
        ]
      ] ++ (List.indexedMap roundDisplay (List.reverse rounds))
        ++ (gameScore model))
