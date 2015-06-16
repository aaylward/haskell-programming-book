module Chapter4 where

-- Question 1
-- define a function halve :: [a] -> ([a], [a])
-- that splits an even-lengthed list into two halves
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-- Question 2
-- define a function safetail :: [a] -> [a] that behaves
-- as the library function `tail`, except that safetail
-- maps the empty list to itself


safetailConditional :: [a] -> [a]
safetailGuards :: [a] -> [a]
safetailPatternMatching :: [a] -> [a]

-- a) conditional
safetailConditional xs = if null xs
  then []
  else tail xs

-- b) guards
safetailGuards xs | null xs = []
            | otherwise = tail []

-- c) pattern matching
safetailPatternMatching [] = []
safetailPatternMatching (x : xs) = xs

-- Question 3
-- define v (or) in 4 ways using pattern matching
or1 :: Bool -> Bool -> Bool
or2 :: Bool -> Bool -> Bool
or3 :: Bool -> Bool -> Bool
or4 :: Bool -> Bool -> Bool

-- 1
True `or1` True = True
True `or1` False = True
False `or1` True = True
False `or1` False = False

-- 2
True `or2` _ = True

-- 3
a `or3` b = if a == b then a else True

-- 4
False `or4` False = False
_ `or4` _ = True

-- Question 4
-- define the following using a conditional expression
-- True ∧ True = True
-- _ ∧ _ = False

and1 :: Bool -> Bool -> Bool
a `and1` b = if a == b && a == True then True else False

-- Question 5
-- define the following using a conditional expression
-- True ∧ b = b
-- False ∧ _ = False

and2 :: Bool -> Bool -> Bool
a `and2` b = if a then b else False

-- Question 6
-- show how the curried function definition
-- mult x y z = x * y *z 
-- can be understood in terms of lambda expressions

mult :: Num a => t -> t1 -> t2 -> a -> a -> a -> a
mult x y z = (\ x -> (\ y -> (\ z -> x * y * z)))

