-- Requires unordered-containers and hashable
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
import Data.Char
import Data.Maybe
import qualified Data.HashSet as HS
import Data.Hashable

newtype Item
  = Item Char
  deriving (Show, Eq, Hashable)

mkItem :: Char -> Maybe Item
mkItem c
  | 'a' <= c && c <= 'z' = Just (Item c)
  | 'A' <= c && c <= 'Z' = Just (Item c)
  | otherwise = Nothing

priority :: Item -> Int
priority (Item c)
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 1 + 26

data Rucksack
  = Rucksack { leftCompartment :: HS.HashSet Item
             , rightCompartment :: HS.HashSet Item }
  deriving (Show, Eq)

parseRucksack :: String -> Rucksack
parseRucksack str = fromMaybe (error "invalid syntax") $ do
  left <- sequence (mkItem <$> leftStr)
  right <- sequence (mkItem <$> rightStr)
  pure (Rucksack (HS.fromList left) (HS.fromList right))
  where
    n = length str
    (leftStr, rightStr) = splitAt (n `div` 2) str

findCommonItem :: Rucksack -> Item
findCommonItem rucksack =
  case n of
    0 -> error "there is no common item"
    1 -> head (HS.toList intersex)
    n -> error "there is more than one common item"
  where
    intersex = HS.intersection (leftCompartment rucksack) (rightCompartment rucksack)
    n = HS.size intersex

main :: IO ()
main = do
  lines <- filter (not . null) . lines <$> readFile "input.txt"
  print (sum (priority . findCommonItem . parseRucksack <$> lines))