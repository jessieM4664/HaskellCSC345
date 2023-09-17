module Prog5 where

import Data.List

--  function reverse' that reverses a list. You must use a case expression inside of your function definition.
reverse' :: [a] -> [a]
reverse' x = case x of
   [] -> []
   (x:xs) -> reverse' xs ++ [x]

-- function isPalindrome that returns if some list can be read the same way forward and backward.
isPalindrome :: String -> Bool
isPalindrome [] = False
isPalindrome [_] = True
isPalindrome x = if (reverse' x == x) then True else False

type TimeStamp = (Int, Int, Int)

-- function isShorter that takes two timestamps and returns 1 if the first is shorter, -1 if the second is shorter, and 0 if they are the same duration.
isShorter :: TimeStamp -> TimeStamp -> Int
isShorter x y
  | (totalSeconds x) < (totalSeconds y) = 1
  | (totalSeconds x) > (totalSeconds y) = -1
  | (totalSeconds x) == (totalSeconds y) = 0

-- function totalSeconds that returns the number of total seconds that a video clip spans.
totalSeconds :: TimeStamp -> Int
totalSeconds (fst, sec, thd) = thd + (sec*60) + (fst*60*60)

-- function isValid that returns whether a time stamp is valid: (1) is not negative in any of its parts, (2) there are no more than 59 seconds, and (3) there are no more than 59 minutes in its representation.
isValid :: TimeStamp -> Bool
isValid (fst, sec, thd)
  | ((thd <= 59 && thd >= 0) && (sec <= 59 && sec >=0) && (fst <= 99 && fst >= 0)) = True
  | otherwise = False

-- function time2Str that returns a string representation of timestamp in the form HH:MM:SS.
time2Str :: TimeStamp -> String
time2Str (fst, sec, thd) = 
    (if (fst > 9) then show fst else "0" ++ show fst) ++ ":" ++
    (if (sec > 9) then show sec else "0" ++ show sec) ++ ":" ++
    (if (thd > 9) then show thd else "0" ++ show thd)


type Date = (Int, Int, Int)

data Season = Winter | Spring | Summer | Fall
  deriving (Show, Eq)

-- function monthLookup that takes a numeric day in the calendar year (between 1 and 365) and returns what month that day is in (excluding leap years).
monthLookup :: Int -> Int
monthLookup x
  | x > 0 && x <= 31 = 1  -- Jan
  | x > 31 && x <= 60 = 2  -- Feb
  | x > 60 && x <= 91 = 3  -- Mar
  | x > 91 && x <= 121 = 4  -- Apr
  | x > 121 && x <= 152 = 5  -- May
  | x > 152 && x <= 182 = 6  -- Jun
  | x > 182 && x <= 213 = 7  -- Jul
  | x > 213 && x <= 244 = 8  -- Aug
  | x > 244 && x <= 274 = 9  -- Sep
  | x > 274 && x <= 305 = 10  -- Oct
  | x > 305 && x <= 325 = 11  -- Nov
  | x > 325 && x <= 356 = 12  -- Dec

-- unction monthRange that takes two numeric days (from previous problem) and returns an integer list of the months between those dates.
monthRange :: Int -> Int -> [Int]
monthRange x y
  | y < x = []
  | x == y = []
  | x < y = nub [i | i <- [monthLookup x | x <- [x..y]]]

-- function validDate that takes a date and returns whether it is valid (e.g. November 31 is not valid). Do not be concerned about leap years.
validDate :: Date -> Bool
validDate (mon, day, yea)
  | mon == 1  && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- Jan
  | mon == 2  && day >= 1 && day <= 28 && yea <= 9999 && yea >= 1000  = True  -- Feb
  | mon == 3  && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- Mar
  | mon == 4 && day >= 1 && day <= 30 && yea <= 9999 && yea >= 1000  = True  -- Apr
  | mon == 5 && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- May
  | mon == 6 && day >= 1 && day <= 30 && yea <= 9999 && yea >= 1000  = True  -- Jun
  | mon == 7 && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- Jul
  | mon == 8 && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- Aug
  | mon == 9 && day >= 1 && day <= 30 && yea <= 9999 && yea >= 1000  = True  -- Sep
  | mon == 10 && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- Oct
  | mon == 11 && day >= 1 && day <= 30 && yea <= 9999 && yea >= 1000  = True  -- Nov
  | mon == 12 && day >= 1 && day <= 31 && yea <= 9999 && yea >= 1000  = True  -- Dec
  | otherwise = False

-- function season that takes a date and returns the season that the date is in.
season :: Date -> Season
season (mon, day, yea)
  | mon >= 3  && mon <= 6 && day >= 1 && day <= 21 && yea <= 9999 && yea >= 1000  = Spring
  | mon >= 6  && mon <= 9  && day >= 1 && day <= 21 && yea <= 9999 && yea >= 1000  = Summer
  | mon >= 9  && mon <= 12  && day >= 1 && day <= 21 && yea <= 9999 && yea >= 1000  = Fall
  | mon >= 1  && mon <= 3  && day >= 1 && day <= 21 && yea <= 9999 && yea >= 1000  = Winter
