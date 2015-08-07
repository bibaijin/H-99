myLast :: [a] -> a
myLast [] = error "No last element for empty lists!"
myLast [x] = x
myLast (_:xs) = myLast xs
