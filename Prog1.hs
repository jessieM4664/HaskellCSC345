module Prog1 where

-- function isThreeDigitPositive that returns whether an integer is a 
-- three digit positive number. isThreeDigitPositive :: Int -> Bool
isThreeDigitPositive :: Int -> Bool -- Complete
isThreeDigitPositive a = (a>=100 && a<=1000)

-- function dividesEvenly that returns whether the second integer divides 
-- evenly into the first integer. dividesEvenly 10 3 should return False.
dividesEvenly :: Integer -> Integer -> Bool
dividesEvenly :: Integer -> Integer -> Bool -- Complete
dividesEvenly a b = mod a b == 0

-- function middle that returns the 2nd greatest of three given int arguments.
middle :: Integer -> Integer -> Integer -> Integer -- Complete
middle x y z
  | x>y && y<z && z>x = x
  | x<y && y>z && z<x = x
  | x>y && y>z && z<x = y
  | x<y && y<z && z>x = y
  | x<y && y>z && z>x = z
  | x>y && y<z && z<x = z
  | x==y || y==z = y
  | x==z = x

-- function nor that computes the NOR gate of two boolean arguments. 
nor :: Bool -> Bool -> Bool -- Complete
nor x y = not(x || y)

-- function f2c that converts a temperature in Fahrenheit into Celcius.
f2c :: Float -> Float -- Complete
f2c f = (f-32)*(5/9)

-- function floorDecimal that calculates the floor of a float, but 
-- returns it as a float rather than an integer.
floorDecimal :: Float -> Float -- Complete
floorDecimal a = fromIntegral(floor a)

-- function letterGrade that returns the equivalent letter grade 
-- for a given numerical integer grade, per the syllabus for this course.
letterGrade :: Integer -> String -- Complete
letterGrade n
  | n <= 60 = "F"
  | n >=60 && n <= 62 = "D-"
  | n >=63 && n <= 66 = "D"
  | n >=67 && n <= 69 = "D+"
  | n >=70 && n <= 72 = "C-"
  | n >=73 && n <= 76 = "C"
  | n >=77 && n <= 79 = "C+"
  | n >=80 && n <= 82 = "B-"
  | n >=83 && n <= 86 = "B"
  | n >=87 && n <= 89 = "B+"
  | n >=90 && n <= 92 = "A-"
  | n >=93 && n <= 100 = "A"

-- function averageThree to return the average of three integers.
averageThree :: Integer -> Integer -> Integer -> Float -- Complete
averageThree a b c = (fromIntegral (a + b + c))/3

-- function howManyAboveAverage that returns how many of three integer inputs are above its average value. 
howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c
  | (a <= (floor(averageThree a b c)) && b <= (floor(averageThree a b c)) && c <= (floor(averageThree a b c))) = 0
  | (a > (floor(averageThree a b c)) && b <= (floor(averageThree a b c)) && c <= (floor(averageThree a b c))) = 1
  | (a <= (floor(averageThree a b c)) && b > (floor(averageThree a b c)) && c <= (floor(averageThree a b c))) = 1
  | (a <= (floor(averageThree a b c)) && b <= (floor(averageThree a b c)) && c > (floor(averageThree a b c))) = 1
  | (a > (floor(averageThree a b c)) && b > (floor(averageThree a b c)) && c <= (floor(averageThree a b c))) = 2
  | (a <= (floor(averageThree a b c)) && b > (floor(averageThree a b c)) && c > (floor(averageThree a b c))) = 2
  | (a > (floor(averageThree a b c)) && b <= (floor(averageThree a b c)) && c > (floor(averageThree a b c))) = 2
  | (a > (floor(averageThree a b c)) && b > (floor(averageThree a b c)) && c > (floor(averageThree a b c))) = 3
