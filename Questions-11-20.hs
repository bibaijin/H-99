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
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "removeAt: the list should not be empty"
removeAt n (x:xs)
  | n == 1 = (x, xs)
  | n > 1 = (element, x : xs')
  where
    (element,xs') = removeAt (n - 1) xs
