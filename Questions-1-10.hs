-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No last element for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2: Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "No the last but one element of empty lists!"
myButLast [_] = error "No the last but one element of singleton!"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:_) 1 = x
elementAt (_:xs) k
  | k < 1 = error "Index out of bounds"
  | otherwise = elementAt xs (k - 1)

-- Problem 4: Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read
-- forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- Problem 7: Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List xs) = foldr (\x acc -> flatten x ++ acc) [] xs
