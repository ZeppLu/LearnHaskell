-- LYH Chapter 6. Higher Order Functions --


-- When dealing with list of lists, it's better to use
-- map / filter than list comprehension
cubic1 xss = map (map (^3)) xss
cubic2 xss = [[x^3 | x <- xs] | xs <- xss]
-- If the argument is used only at the ending, it's event better to do so
cubic3 = map (map (^3))


-- Make use of lazy evaluation
largestDivisible = head $ filter p [100000,99999..]
    where p n = n `mod` 42 == 0

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Lambdas can also use patter matching, but limited to only one,
-- which means you can't write several patterns
merge :: Num a => [(a, a)] -> [a]
-- (Haskell's type inference is dumb in some way,
-- it infer this function to be `[(Integer, Integer)] -> [Integer]')
merge = map (\(a, b) -> a + b)


-- Fold
-- Dumb again here
sum' :: Num a => [a] -> a
sum' = foldl (+) 0
elem' x xs = foldl (||) False (zipWith (==) (repeat x) xs)
-- A possibly clearer method
elem'' x xs = any (==x) $ xs
-- `map' can also built upon `foldl', but it requires `++',
-- which it much more expansive than `:'
map' f xs = foldr applyNew [] xs
    where applyNew x acc = f x : acc
reverse' xs = foldl reverseNew [] xs
    where reverseNew acc x = x : acc
-- Less readable on
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- `intersperse' can be implemented using `foldx1'
intersperse _  []  = []
intersperse xs xss = foldl1 joinNew xss
    where joinNew acc ys = acc ++ xs ++ ys

-- It seems good pratice to use `scanx' along with `takeWhile'
-- Eg: to find the smallest N that `sum [1..N] > 1000'
findN = length (takeWhile (<=1000) (scanl1 (+) [1..])) + 1

-- Function application: $
-- Lowest precedence, right-associative
findN' = (1+) $ length $ takeWhile (<=1000) $ scanl1 (+) [1..]
-- Some interestion usage
operateOnThree = map ($ 3) [(10+), ((-)8), (/10), (^2), sqrt]

-- Function composition: .
-- Right-associative
findN'' = (1+) $ length . takeWhile (<=1000) . scanl1 (+) $ [1..]
