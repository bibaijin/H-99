-- Problem 14: Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- Problem 19: Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate xs r
  | r > 0 = drop r xs ++ take r xs
  | r <= 0 = drop r' xs ++ take r' xs
  where
    r' = r + length xs

-- Problem 20: Remove the K'th element from a list
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt 1 (x:xs) = (Just x, xs)
removeAt k (x:xs) = (a, x : rest)
  where
    (a,rest) = removeAt (k - 1) xs
