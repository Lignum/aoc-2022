-- Requires unordered-containers, hashable and split
-- runghc --ghc-arg="-package unordered-containers" --ghc-arg="-package hashable" --ghc-arg="-package split" Part2.hs
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
import Data.Char
import Data.Maybe
import Data.List.Split
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

newtype Group
  = Group (HS.HashSet Item)
  deriving (Show, Eq)

parseGroup :: String -> Group
parseGroup str = Group (HS.fromList (fromMaybe (error "invalid syntax") (traverse mkItem str)))

findCommonItem :: Group -> Group -> Group -> Item
findCommonItem (Group g1) (Group g2) (Group g3) =
  case n of
    0 -> error "there is no common item"
    1 -> head (HS.toList intersex)
    n -> error "there is more than one common item"
  where
    intersex = g1 `HS.intersection` g2 `HS.intersection` g3
    n = HS.size intersex

uncurry3 ::  (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  let groups = chunksOf 3 (parseGroup <$> input)

  if length input `mod` 3 /= 0 then
    putStrLn "Amount of lines must be divisible by 3"
  else
    print (sum (priority . uncurry3 findCommonItem . (\[a, b, c] -> (a, b, c)) <$> groups))
