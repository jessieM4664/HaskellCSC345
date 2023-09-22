module Prog6 where

import Data.Char
import Data.List

-- function safeLast that behaves similarly to the built-in last function, except that it returns Nothing if an empty list is passed as an argument.
safeLast :: [Int] -> Maybe Int
safeLast [] = Nothing
safeLast xs = Just (last xs)

-- function safeCount that takes an item to search for and a list of numbers, and returns Nothing if the list is empty; Just 0 if the item doesn't appear in the list; or Just x where x is the number of times that the item appears in the list.
safeCount :: Int -> [Int] -> Maybe Int
safeCount _ [] = Nothing
safeCount n xs = Just (length[x | x <- xs, n==x])

{----------}

data Set345 = NonEmptySet [Int]
            | EmptySet
      deriving Show

-- function singletonOrEmpty that takes a set and returns if it is empty or is a singleton.
singletonOrEmpty :: Set345 -> Bool
singletonOrEmpty EmptySet = True
singletonOrEmpty (NonEmptySet []) = True
singletonOrEmpty (NonEmptySet [_]) = True
singletonOrEmpty (NonEmptySet [_,_]) = False
singletonOrEmpty (NonEmptySet [_,_,_]) = False

-- function member that checks whether the given item is present in the given set.
member :: Int -> Set345 -> Bool
member _ EmptySet = False
member _ (NonEmptySet []) = False
member n (NonEmptySet (x:xs)) = if (x==n) then True else member n (NonEmptySet xs)

-- function size that returns the number of elements in a given set.
size :: Set345 -> Int
size EmptySet = 0
size (NonEmptySet []) = 0
size (NonEmptySet xs) = length xs

-- function ins that inserts the given item into a set. (If the item is already in the set, simply return the set unmodified.)
ins :: Int -> Set345 -> Set345
ins n EmptySet = NonEmptySet [n]
ins n (NonEmptySet xs)
  | elem n xs    = NonEmptySet xs
  | otherwise    = NonEmptySet (merge n xs)
  where merge n [] = [n]
        merge n (y:ys)
          | n < y = n:y:ys
          | otherwise = y : merge n ys

-- function union' that takes two sets and returns the union of both sets.
union' :: Set345 -> Set345 -> Set345
union' EmptySet EmptySet = EmptySet
union' EmptySet (NonEmptySet []) = EmptySet
union' (NonEmptySet []) EmptySet = EmptySet
union' (NonEmptySet []) (NonEmptySet []) = EmptySet
union' (NonEmptySet xs) EmptySet = NonEmptySet xs
union' EmptySet (NonEmptySet ys) = NonEmptySet ys
union' (NonEmptySet xs) (NonEmptySet ys) = NonEmptySet (nub (sort (xs ++ ys)))

-- function intersection' that takes two sets and returns the intersection of both sets.
intersection' :: Set345 -> Set345 -> Set345
intersection' EmptySet EmptySet = EmptySet
intersection' EmptySet (NonEmptySet []) = EmptySet
intersection' (NonEmptySet []) EmptySet = EmptySet
intersection' (NonEmptySet []) (NonEmptySet []) = EmptySet
intersection' (NonEmptySet xs) EmptySet = NonEmptySet xs
intersection' EmptySet (NonEmptySet ys) = NonEmptySet ys
intersection' (NonEmptySet xs) (NonEmptySet ys)
  | ((dropSingles(sort (xs ++ ys)))) == []    = EmptySet
  | otherwise = NonEmptySet (dropSingles(sort (xs ++ ys)))

dropSingles [] = []
dropSingles (x:y:rest) | x == y = x:dropSingles (dropWhile (== x) rest)
dropSingles (x:rest) = dropSingles rest

{----------}

-- function countLetters that inputs three Strings from the user on separate lines
countLetters :: IO [Int]
countLetters = do
  putStr "Enter 3 strings: "
  str1 <- getLine
  str2 <- getLine
  str3 <- getLine
  return [(length str1),(length str2),(length str3)]

-- function and' that takes as an argument a Boolean value and performs a conjunction ("ands it") with an inputed String "True" or "False" from the user, and returns the Boolean value result.
and' :: Bool -> IO Bool 
and' x = do
   putStrLn "Enter Bool (True/False): "
   str <- getLine
   let x' = read str :: Bool
   return (x && x')
