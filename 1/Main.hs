parseInput :: [String] -> [[Int]]
parseInput lines = go lines []
  where
    go [] ys = [ys]
    go ("":xs) ys = ys : go xs []
    go (x:xs) ys = go xs (read x : ys) 

main :: IO ()
main = readFile "input.txt" >>= \input ->
  print (maximum (sum <$> parseInput (lines input)))
