module Prog3 where

import Data.Char

-- function absAll that takes a list of ints, and returns a list of pairs, such that the first element in each pair is in the original number, and the second element is the absolute value of the original number
absAll :: [Int] -> [(Int, Int)]
absAll xs = zip xs ([if x<0 then x*(-1) else x | x <- xs])

-- function flip' that takes a list of pairs, and returns a list of pairs, with the pairs flipped (the first item becomes the second item, and vice versa).
flip' :: [(Int, Int)] -> [(Int, Int)]
flip' xs = [(x,y) | (y,x) <- xs]

-- function orderTriple that takes a triple, and returns a version in decreasing order. (Hint: you may want to define other helper functions such as maxOfThree, middleOfThree, and minOfThree.)
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x,y,z) 
  | x > y && y > z && z < x = (x,y,z)
  | x > y && y < z && z < x = (x,z,y)
  | x < y && y > z && z < x = (y,x,z)
  | x < y && y > z && z > x = (y,z,x)
  | x < y && y < z && z > x = (z,y,x)
  | x > y && y < z && z > x = (z,x,y)
  | x == y && z > x         = (z,x,x) -- 2
  | y == z && x > z         = (x,y,y) -- 2
  | z == x && y > x         = (y,z,z) -- 2
  | z == x && y < x         = (z,z,y) -- 2
  | x == y && z < x         = (x,x,z) -- 2
  | x == y && y == z        = (x,x,x) -- 3

-- function asciiNums that takes a String and returns a list of the ascii values of characters in that string
asciiNums :: String -> [Int]
asciiNums str = [fromEnum x | x <- str]

-- function triads that generates a list of integer triples.
triads :: Int -> [(Int,Int,Int)]
triads n = [(x,y,z) | x <- [1..n],y <- [1..n],z <- [1..n], x<=n && y<=n && z<=n && ((x*x)+(y*y)==(z*z))]

-- function sumLastPart which, only using list library functions (no list comprehension), returns the sum of the last 
 numbers in the list, where n is the first argument to the function. 
sumLastPart :: Int -> [Int] -> Int
sumLastPart n xs = sum (drop ((length xs) - n) xs)

-- function middleProduct which, only using list library functions (no list comprehension), returns the product of the interior items in the list, that is, everything except the first and last item. Assume there are always at least three numbers in the list.
middleProduct :: [Int] -> Int
middleProduct xs
  | (length xs) < 3    = xs !! 2
  | otherwise          = product (drop 1 (take ((length xs)-1) xs))

--  function init' that has identical behavior to the init function. In your definition, you may only use list library functions that we covered (no list comprehension).
init' :: [Int] -> [Int]
init' xs = take ((length xs)-1) xs

-- function lowerFirstCharacter that lowercases the first character in a string.
lowerFirstCharacter :: String -> String
lowerFirstCharacter str
  | [x | x <- ['a','b'..'z'], x == head str] /= []    = str
  | otherwise     = toLower (head str) : (tail str)

-- function elemAt that returns the ith item of the list, where the first item is index 1.
elemAt :: Int -> [Int] -> Int
elemAt x xs = sum (drop (x-1) (take x xs))
