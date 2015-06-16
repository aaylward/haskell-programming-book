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


safetail :: [a] -> [a]

-- a) conditional
safetail xs = if null xs
  then []
  else tail xs

-- b) guards
safetail xs | null xs = []
            | otherwise = tail []

-- c) pattern matching
safetail [] = []
safetail (x : xs) = xs

-- Question 3
-- define v (or) in 4 ways using pattern matching
v :: Bool -> Bool -> Bool

-- 1
v True True = True
v True False = True
v False True = True
v False False = False

-- 2
v True _ = True

-- 3
v a b = if a == b then a else True

-- 4
v False False = False
v _ _ = True

-- Question 4
-- define the following using a conditional expression
-- True ∧ True = True
-- _ ∧ _ = False

(∧) :: Bool -> Bool -> Bool
a ∧ b = if a == b && a == True then True else False

-- Question 5
-- define the following using a conditional expression
-- True ∧ b = b
-- False ∧ _ = False

a ∧ b = if a then b else False

-- Question 6
-- show how the curried function definition
-- mult x y z = x * y *z 
-- can be understood in terms of lambda expressions

mult :: Int -> Int -> Int -> Int
mult x y z = λx -> (λy -> (λz -> x * y * z))

