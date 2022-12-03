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

simulateRound :: Shape -> Shape -> Round
simulateRound sel opp = Round outcome sel opp where
  outcome =
    case (sel, opp) of
      (Rock, Paper) -> Lose
      (Rock, Scissors) -> Win
      (Rock, Rock) -> Draw
      (Paper, Paper) -> Draw
      (Paper, Scissors) -> Lose
      (Paper, Rock) -> Win
      (Scissors, Paper) -> Win
      (Scissors, Scissors) -> Draw
      (Scissors, Rock) -> Lose

parseInput :: [String] -> [(Shape, Shape)]
parseInput [] = []
parseInput ("":lns) = parseInput lns
parseInput ((a:' ':b:cs):lns) =
  let opponent =
        case a of
          'A' -> Rock
          'B' -> Paper
          'C' -> Scissors
      selected =
        case b of
          'X' -> Rock
          'Y' -> Paper
          'Z' -> Scissors
    in (selected, opponent) : parseInput lns
parseInput (ln:_) = error $ "invalid syntax on line " ++ ln

main :: IO ()
main = readFile "input.txt" >>= (\input ->
  print (sum (roundScore . uncurry simulateRound <$> parseInput input))) . lines