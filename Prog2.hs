module Prog2 where

-- fnction tripleNumber that multiplies a number by 3 but only if that number is smaller than or equal to 100.
tripleNumber :: Integer -> Integer
tripleNumber x
  | (x * 3) <= 100 = x * 3
  | otherwiseu = x

-- function twoSame that returns True if at least two of the three arguments are equal, and False otherwise.
twoSame :: Integer -> Integer -> Integer -> Bool
twoSame x y z = (x==y) || (y==z) || (z==x)

-- function trangleArea that computers the area of a triangle given its base and height.
triangleArea :: Integer -> Integer -> Float
triangleArea b h = (fromIntegral(b * h))/2

-- function product' that that uses recursion to compute the product of all numbers from 1 to n, the argument. Assume that n is greater than or equal to 1.
product' :: Integer -> Integer
product' n
  | n == 0 = 1
  | n > 0 = product' (n-1) * n

-- Redefine the built-in infix || operator. Name your new function or' with prefix notation. Do not use guards in your definition, but rather utilize pattern matching. 
-- Use the wildcard _ pattern so that you only have two definitions, rather than the four definitions we would usually write given the truth table of ||.
or' :: Bool -> Bool -> Bool
or' False False = False
or' _ x = True

-- function integerSqrt that returns the integer square root of a positive integer n. (The integer square root is defined to be the largest integer whose square is 
-- less than or equal to n, i.e. the result of integerSqrt 15 is 3.).
integerSqrt :: Integer -> Integer
integerSqrt x = floor (sqrt (fromIntegral x))
 
-- function exponent' that recursively computes the result of raising some base number, b, to some exponent, e. (e.g. 2^8 = 64). You may not use the ^ or ** operators 
-- you must use recursion. This function will only be called on an exponent value that is a whole number (an integer that is 0 or greater). 
exponent' :: Integer -> Integer -> Integer
exponent' n p
  | p < 1     = 1
  | p > 0     = n * exponent' n (p-1)

-- function swap that swaps the characters in a quintuple (5-tuple) in the following way: the first element swaps with the last, the second swaps with the fourth, 
-- and the middle doesn't change. Only use pattern matching. You may not call any other functions.
swap :: (Char, Char, Char, Char, Char) -> (Char, Char, Char, Char, Char)
swap (a,b,c,d,e) = (e,d,c,b,a)

-- function negateTwoDigits that uses list comprehension to take a list of integers and return a list of integers with all of the two-digit integers negated.
negateTwoDigits :: [Integer] -> [Integer]
negateTwoDigits llist = [if x>9&&x<100 || x<(-9)&&x>(-100) then x*(-1) else x | x <- llist]

-- function matches that uses list comprehension to pick out all instances of an integer n from a list.
matches :: Integer -> [Integer] -> [Integer]
matches n llist = [x | x <- llist, x == n]

-- Use the matches function in the above problem to write a function element that uses list comprehension to return True if an element is a member of a list, False otherwise.
element :: Integer -> [Integer] -> Bool
element n llist = [x | x <- llist, x == n] /= []
