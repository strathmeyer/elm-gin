module Model
  exposing
    ( GameState(..)
    , Model
    , Player(..)
    , Round
    , Score
    , SingleScore
    , init
    , playerScore
    , roundInit
    , sumScores
    )

type GameState = InProgress | Completed

-- TODO: use player names
type alias Model =
    { playerOneName : String
    , playerTwoName : String
    , rounds : List Round
    , state : GameState
    , roundTotal : Score
    , boxTotal : Score
    , total : Score
    }


sumScores : List Score -> Score
sumScores scores =
  let
    playerTotal player =
      List.sum (List.map (playerScore player) scores)

  in
    [ SingleScore PlayerOne (playerTotal PlayerOne)
    , SingleScore PlayerTwo (playerTotal PlayerTwo)
    ]


type Player = PlayerOne | PlayerTwo


type alias SingleScore =
  { player : Player
  , score : Int
  }


type alias Score = List SingleScore


twoPlayerScoreInit =
  [
    SingleScore PlayerOne 0,
    SingleScore PlayerTwo 0
  ]


findScore : Player -> Score -> Maybe SingleScore
findScore player score =
  List.head (List.filter (\n -> n.player == player) score)


playerScore : Player -> Score -> Int
playerScore player score =
  case findScore player score of
    Nothing ->
      0
    Just score ->
      score.score


type alias Round =
  { dealer : Player
  , knocker : Maybe Player
  , winner : Maybe Player -- not nec the knocker if undercut
  , deadwood : Score
  , score : Score
  }


roundInit : Player -> Round
roundInit dealer =
  Round
    dealer
    Nothing
    Nothing
    twoPlayerScoreInit
    twoPlayerScoreInit


init : Model
init =
  Model
    ""
    ""
    [ roundInit PlayerOne ]
    InProgress
    twoPlayerScoreInit
    twoPlayerScoreInit
    twoPlayerScoreInit
