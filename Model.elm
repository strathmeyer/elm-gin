module Model
  exposing
    ( Model
    , Player(..)
    , Round
    , Score
    , init
    , playerScore
    , roundInit
    , sumScores
    )

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

-- TODO: rename Score to something like SingleScore
-- so we can then alias Score to List SingleScore and
-- stop using List Score so much.
sumScores : List Round -> List Score
sumScores rounds =
  let
    scores =
      List.map .score rounds

    playerTotal player =
      List.sum (List.map (playerScore player) scores)

  in
    [ Score PlayerOne (playerTotal PlayerOne)
    , Score PlayerTwo (playerTotal PlayerTwo)
    ]


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


findScore : Player -> List Score -> Maybe Score
findScore player scoreList =
  List.head (List.filter (\n -> n.player == player) scoreList)


playerScore : Player -> List Score -> Int
playerScore player scoreList =
  case findScore player scoreList of
    Nothing ->
      0
    Just score ->
      score.score


type alias Round =
  { dealer : Player
  , knocker : Maybe Player
  , winner : Maybe Player -- not nec the knocker if undercut
  , deadwood : List Score
  , score : List Score
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
