
module Prog4 where

import Data.Char

-- recursive function tripleAll that takes a list of ints, and returns a list of pairs, such that the first element in each pair is in the original number, and the second element is the original number tripled.
tripleAll :: [Int] -> [(Int, Int)]
tripleAll [] = []
tripleAll (x:xs) = (x, 3*x) : tripleAll xs

-- recursive function flip' that takes a list of pairs, and returns a list of pairs, with the pairs flipped.
flip' :: [(Int, Int)] -> [(Int, Int)]
flip' [] = []
flip' ((x,y):xs) = (y,x) : flip' xs

-- recursive function sumLastPart that returns the sum of the last numbers in the list, where n is the first argument to the function.
sumLastPart :: Int -> [Int] -> Int
sumLastPart _ [] = 0
sumLastPart 0 _  = 0
sumLastPart n (x:xs) = sum (drop ((length xs) - n) xs)

-- recursive function middleProduct that returns the product of the interior items in the list, that is, everything except the first and last item.
middleProduct :: [Int] -> Int
middleProduct [] = 0
middleProduct [_,x,_] = x
middleProduct (x:xs) = product (init xs)

-- recursive function init' that has identical behavior to the init function.
init' :: [Int] -> [Int]
init' [_] = []
init' (x:xs) = x : init' xs

-- recursive function lowerOddLetters that lowercases the first, third, fifth letter (and so on) of a string.
lowerOddLetters :: String -> String
lowerOddLetters [] = []
lowerOddLetters xs = if (tail xs == []) then (if (fromEnum (head xs) > 64 && fromEnum (head xs) < 91) then (toEnum (fromEnum (head xs) + 32) :: Char):lowerOddLetters (tail xs) 
                    else head (xs):[])
                    else if (fromEnum (head xs) > 64 && fromEnum (head xs) < 91) then (toEnum (fromEnum (head xs) + 32) :: Char):head (tail xs):lowerOddLetters (tail (tail xs)) 
                    else head xs:head (tail xs):lowerOddLetters (tail (tail xs))

--  recursive function elemAt that returns the ith item of the list, where the first item is index 1.
elemAt :: Int -> [Int] -> Int
elemAt _ [] = 0
elemAt 1 (x:_) = x
elemAt n (x:xs) = elemAt (n-1) xs

-- recursive function iSort' that uses insertion sort to sort a list of pairs (String, Int) where only the first element.
iSort' :: [(String, Int)] -> [(String, Int)]
iSort' [] = []
iSort' (x:xs) = insert' x (iSort' xs)
   where
      insert' :: (String, Int) -> [(String, Int)] -> [(String, Int)]
      insert' x [] = [x]
      insert' (s, n) ((s', n') : xs)
        | n < n' = (s, n) : (s', n') : xs
        | otherwise = (s', n') : (insert' (s, n) xs)

-- recursive function middleWord that that returns the second word in a string that is composed of exactly three words.
middleWord :: String -> String
middleWord [] = []
middleWord xs = if (head xs /= ' ') then middleWord (tail xs) else if (head (drop (length xs - 1) xs) /= ' ') then middleWord (take (length xs - 1) xs) else reverse (tail (reverse (tail xs)))

-- recursive function lowerFirstLetter that lowercases the first uppercase letter in a string. 
lowerFirstLetter :: String -> String
lowerFirstLetter [] = []
lowerFirstLetter xs = if ([i | i <- [0 .. ((length xs) - 1)], fromEnum(xs!!i) > 64, fromEnum(xs!!i) < 91] == []) then xs else if (fromEnum (head xs) > 64 && fromEnum (head xs) < 91) 
                     then (toEnum (fromEnum (head xs) + 32) :: Char):tail xs else head xs:lowerFirstLetter (tail xs)
