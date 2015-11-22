-- Takes two tuples as input and outputs a new tuple that combines the two values of the functions.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Takes a function and a value, if the value is empty the function returns Nothing. Otherwise it returns the result of the function with the input value.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes two inputs, if the first argument is Nothing it returns x. If the first argument is something, the other can be anything and the first one is returned.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- The maybe function takes a default value, a function, and a Maybe value. If the Maybe value is Nothing, the function returns the default value. Otherwise, it applies the function to the value inside the Just and returns the result.    
-- Tries a function and sees if it returns a certain value?
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Takes a function and a value and applies the function to the value. If the function returns the same value, the value is returned. Otherwise it is recursively tried again until it does.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- The value r needs to be a member of RealFrac. Take a RealFrac and a list. Checks the length of the list. Transforms the Integral to Num. Something... returns the greatest integer not greater than argument. Then takes this element from the list.
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs