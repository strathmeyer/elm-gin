module Model exposing (..)

--Game State:
--  name - player1 (first dealer)
--  name - player2
--  list of  [Rounds]
--  state (in progress, completed)
--  roundsTotal Score
--  boxTotal Score
--  total Score
type GameState = InProgress | Completed

type alias Model =
    { playerOneName : String
    , playerTwoName : String
    , rounds : List Round
    , state : GameState
    , roundTotal : List Score
    , boxTotal : List Score
    , total : List Score
    }

type Player = PlayerOne | PlayerTwo

type alias Score =
  { player : Player
  , score : Int
  }

twoPlayerScoreInit =
  [
    Score PlayerOne 0,
    Score PlayerTwo 0
  ]

--playerOneScore : [ Score ] -> Int
--playerOneScore =


type alias Round =
  { dealer : Player
  , knocker : Maybe Player
  , winner : Maybe Player -- not nec the knocker if undercut
  , deadwood : List Score
  , score : List Score
  }

init : Model
init =
    Model
      ""
      ""
      [ ]
      InProgress
      twoPlayerScoreInit
      twoPlayerScoreInit
      twoPlayerScoreInit
