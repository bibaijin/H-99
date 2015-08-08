-- Problem 1:
myLast :: [a] -> a
myLast [] = error "No last element for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2:
myButLast :: [a] -> a
myButLast [] = error "No the last but one element of empty lists!"
myButLast [_] = error "No the last but one element of singleton!"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3:
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) k
  | k < 1 = error "Index out of bounds"
  | otherwise = elementAt xs (k - 1)

-- Problem 4:
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

-- Problem 5:
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6:
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7: Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List xs) = foldr (\x acc -> flatten x ++ acc) [] xs
