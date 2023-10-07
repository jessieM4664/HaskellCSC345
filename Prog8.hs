module Prog8 where

-- function sumSqNeg that computes the "sum of squares of negatives".
sumSqNeg :: [Int] -> Int
sumSqNeg xs = foldr (+) 0 (map (\x -> x*x) (filter (\x -> x<0) xs))

-- function containing (without any higher order functions) that returns whether each element in the first list is also in the second list.
containing :: Eq a => [a] -> [a] -> Bool
containing [] _      = True 
containing (x:xs) ys = (elem x ys) && (containing xs ys)

-- function total that applies the function (first argument) to every element in the list (second argument) and sums the result.
total :: (Int -> Int) -> [Int] -> Int
total f xs = foldr (+) 0 (map f xs)

-- function containing' (with higher order functions) that returns whether each element in the first list is also in the second list.
containing' :: Eq a => [a] -> [a] -> Bool
containing' xs ys = length (filter (\x -> elem x ys) xs) == length xs

-- function lengths that returns a list of lengths of the given strings.
lengths :: [String] -> [Int]
lengths xs = map length xs

-- function product' that returns the product of a nonempty list of numbers. 
product' :: Num a => [a] -> a
product' xs = foldr (*) 1 xs

-- function max' that returns the largest element of a nonempty list of two digit numbers.
max' :: Num a => Ord a => [a] -> a
max' xs = foldr max (head xs) xs

-- function append' that appends two lists.
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

-- function filterFirst that removes the first element from the list (second argument) that does not satisfy a given predicate function.
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = [] 
filterFirst p (x:xs)
  | p x       = x : filterFirst p xs
  | otherwise = xs
