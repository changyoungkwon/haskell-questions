
-- problem 1
myLast :: [a] -> a
myLast [a] = a
myLast (a:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a 
myButLast = last . init

-- problem 3
elementAt :: [a] -> Int -> a
elementAt xs a = xs !! (a - 1)

-- problem 4
myLength :: [a] -> Int 
myLength [a] = 1
myLength (a:xs) = 1 + myLength xs

-- problem 5
myReverse :: [a] -> [a]
myReverse (a:xs) = myReverse xs ++ [a]


-- problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome x = (head x == last x) && isPalindrome (init $ tail x)

-- problem 7

-- problem 8
compress :: (Eq a) => [a] -> [a]
compress [a] = [a]
compress (a:xs)
    | a == head xs = compress xs
    | otherwise     = [a] ++ compress xs

-- problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [a] = [[a]]
pack (a:xs) = if a `elem` (head (pack xs))
            then (a:(head (pack xs))):tail (pack xs)
            else [a]:(pack xs)

-- problem 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode [a] = [(1, a)]
encode (a:xs) = if a == snd (head (encode xs))
            then (1 + fst (head (encode xs)), a):tail (encode xs)
            else (1, a):encode xs
     


main = print $ encode "abbbbbbbbaba"