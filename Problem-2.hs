myButLast :: [a] -> a
myButLast [] = error "No the last but one element of empty lists!"
myButLast [_] = error "No the last but one element of singleton!"
myButLast [x:y] = x
myButLast (_:xs) = myButLast xs
