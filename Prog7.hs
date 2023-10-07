module Prog7 where

-- function unique that returns the list of elements that occur exactly once in the argument list. You must use recursion and not list comprehension.
unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs)
        | elem x (unique xs) = [y | y <- (unique xs), y /= x ]
        | otherwise                = x : (unique xs)

isVowel :: Char -> Bool
isVowel c = elem c ['u','e','o','a','i']

-- function piglatinize that returns a word into its piglatin form: if it begins with a vowel, add to the end "yay", else move non-vowels to the end of the string until a vowel is at the front and then add to the end "ay".
piglatinize :: String -> String
piglatinize [] = []
piglatinize (x:xs) = if (isVowel x) then str ++ "yay" 
                      else
                        xs ++ [x] ++ "ay"
                        where str = (x:xs)

data Expr1 = Val1 Int
            | Add1 Expr1 Expr1
            | Sub1 Expr1 Expr1
            | Mul1 Expr1 Expr1
            | Div1 Expr1 Expr1

-- function value1 that evaluates an expression.
value1 :: Expr1 -> Int
value1 (Val1 n) = n
value1 (Add1 e1 e2) = (value1 e1)   +   (value1 e2)
value1 (Sub1 e1 e2) = (value1 e1)   -   (value1 e2)
value1 (Mul1 e1 e2) = (value1 e1)   *   (value1 e2)
value1 (Div1 e1 e2) = (value1 e1) `div` (value1 e2)

-- function value2 that evaluates an expression, but returns Nothing if there is a division by zero scenario.
value2 :: Expr1 -> Maybe Int
value2 (Val1 n) = Just n
value2 (Div1 e1 (Val1 0)) = Nothing
value2 (Add1 e1 e2) = Just (value1 (Add1 e1 e2))
value2 (Sub1 e1 e2) = Just (value1 (Sub1 e1 e2))
value2 (Mul1 e1 e2) = Just (value1 (Mul1 e1 e2))
value2 (Div1 e1 e2) = Just (value1 (Div1 e1 e2))

instance Show Expr1 where
    show (Val1 n) = show n
    show (Add1 e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Sub1 e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
    show (Mul1 e1 e2) = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"
    show (Div1 e1 e2) = "(" ++ show e1 ++ "'div'" ++ show e2 ++ ")"

data Expr2 = Val2 Int
            | Add2 Expr2 Expr2
            | Sub2 Expr2 Expr2
            | Mul2 Expr2 Expr2
            | Div2 Expr2 Expr2
            | If BExpr2 Expr2 Expr2

data BExpr2 = BoolLit Bool
            | And BExpr2 BExpr2
            | EqualTo Expr2 Expr2
            | GreaterThan Expr2 Expr2

-- function bEval :: BExpr2 -> Bool that evaluates instances of the above boolean expression.
bEval :: BExpr2 -> Bool
bEval (BoolLit b) = b
bEval (And be1 be2) = bEval be1 && bEval be2
bEval (EqualTo e1 e2) = eval e1 == eval e2
bEval (GreaterThan e1 e2) = eval e1 > eval e2

eval :: Expr2 -> Int
eval (Val2 n) = n
eval (Add2 e1 e2) = (eval e1)   +   (eval e2)
eval (Sub2 e1 e2) = (eval e1)   -   (eval e2)
eval (Mul2 e1 e2) = (eval e1)   *   (eval e2)
eval (Div2 e1 e2) = (eval e1) `div` (eval e2)
eval (If b e1 e2) = if bEval b then eval e1 else eval e2

value3 :: Expr2 -> Maybe Int
value3 (Val2 n) = Just n
value3 (Add2 e1 e2) = do
    n1 <- value3 e1
    n2 <- value3 e2
    return (n1 + n2)
value3 (Sub2 e1 e2) = do
    n1 <- value3 e1
    n2 <- value3 e2
    return (n1 - n2)
value3 (Mul2 e1 e2) = do
    n1 <- value3 e1
    n2 <- value3 e2
    return (n1 * n2)
value3 (Div2 e1 e2) = do
    n1 <- value3 e1
    n2 <- value3 e2
    if (n2==0) then Nothing else return (n1 `div` n2)
value3 (If cond e1 e2) = do
    b <- valueB cond
    if b then value3 e1 else value3 e2

valueB :: BExpr2 -> Maybe Bool
valueB (BoolLit b) = Just b
valueB (And b1 b2) = do
    vb1 <- valueB b1
    vb2 <- valueB b2
    return (vb1 && vb2)
valueB (EqualTo e1 e2) = do
    n1 <- value3 e1
    n2 <- value3 e2
    return (n1 == n2)
valueB (GreaterThan e1 e2) = do
    n1 <- value3 e1
    n2 <- value3 e2
    return (n1 > n2)
