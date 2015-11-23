{- substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute '' xs _ = xs
substitute ch xs val = concat [if x == ch then val else [x] | x <- xs]

substitute ch xs val =
	| elem ch xs = val : substitute ch xs val
	| otherwise = x : substitute ch xs val

-}



-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] xs =  Nothing
match _ xs [] = Nothing
match wc (p:ps) (s:ss)
  | wc == p = match wc ps ss
  | otherwise =  orElse (singleWildCardMatch wc ps ss) (longerWildcardMatch p:ps s:ss)



-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = Nothing
{- TO BE WRITTEN -}



longerWildcardMatch (wc:ps) (x:xs) = Nothing
{- TO BE WRITTEN -}