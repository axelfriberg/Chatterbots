import Utilities
import Data.Maybe


substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute ch xs val = concat [if x == ch then val else [x] | x <- xs]
--substitute ch xs val = concatMap (\x -> if x==ch then val else [x])


{-substitute ch xs val =
	| elem ch xs = val : substitute ch xs val
	| otherwise = x : substitute ch xs val
-}




-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] xs =  Nothing
match _ xs [] = Nothing
match wc (p:ps) (x:xs)
  | wc == p =  orElse (singleWildcardMatch (wc:ps) (x:xs)) (longerWildcardMatch (wc:ps) (x:xs))
  | p == x = match wc ps xs
  | otherwise = Nothing

-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]

singleWildcardMatch (wc:ps) (x:xs) 
  | match wc ps xs == Just [] = Just [x]
  | otherwise = Nothing

longerWildcardMatch (wc:ps) (x:xs) 
  | match wc (wc:ps) xs /= Nothing = Just (x: fromJust (match wc (wc:ps) xs))
  | otherwise = Nothing