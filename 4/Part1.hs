import Data.List.Split

data Assignment
  = Assignment (Int, Int) (Int, Int)
  deriving (Show, Eq)

parseAssignment :: String -> Assignment
parseAssignment str =
  Assignment (read a, read b) (read c, read d)
  where
    [l, r] = splitOn "," str
    [a, b] = splitOn "-" l
    [c, d] = splitOn "-" r

fullyContains :: Assignment -> Bool
fullyContains (Assignment (a1, a2) (b1, b2)) =
  a1 <= b1 && a2 >= b2 || b1 <= a1 && b2 >= a2

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let assignments = parseAssignment <$> lines contents
  print (length (filter fullyContains assignments))