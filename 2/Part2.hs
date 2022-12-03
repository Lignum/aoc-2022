data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq, Enum)

data Outcome
  = Win
  | Lose
  | Draw
  deriving (Show, Eq, Enum)

data Round
  = Round Outcome Shape Shape
  deriving (Show, Eq)

shapeScore :: Shape -> Int
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3

roundScore :: Round -> Int
roundScore (Round Win sel _) = shapeScore sel + 6
roundScore (Round Draw sel _) = shapeScore sel + 3
roundScore (Round Lose sel _) = shapeScore sel

simulateRound :: Shape -> Outcome -> Round
simulateRound opp outcome = Round outcome sel opp where
  sel =
    case (opp, outcome) of
      (x, Draw) -> x
      (Rock, Win) -> Paper
      (Rock, Lose) -> Scissors
      (Paper, Win) -> Scissors
      (Paper, Lose) -> Rock
      (Scissors, Win) -> Rock
      (Scissors, Lose) -> Paper

parseInput :: [String] -> [(Shape, Outcome)]
parseInput [] = []
parseInput ("":lns) = parseInput lns
parseInput ((a:' ':b:cs):lns) =
  let opponent =
        case a of
          'A' -> Rock
          'B' -> Paper
          'C' -> Scissors
      outcome =
        case b of
          'X' -> Lose
          'Y' -> Draw
          'Z' -> Win
    in (opponent, outcome) : parseInput lns
parseInput (ln:_) = error $ "invalid syntax on line " ++ ln

main :: IO ()
main = readFile "input.txt" >>= (\input ->
  print (sum (roundScore . uncurry simulateRound <$> parseInput input))) . lines