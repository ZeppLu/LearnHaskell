-- LYH Chapter 2. Starting Out --


-- Differences between `max' and `maximum'
max' a b
    | a > b     = a
    | otherwise = b
-- Empty list is not allowed
maximum' []     = error "cannot obtain maximum value from an empty list"
maximum' [x]    = x
maximum' (x:xs) = max' x (maximum' xs)

-- Differences between `zip' and `zipWith'
zip' []     _      = []
zip' _      []     = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zipWith' f []     _      = []
zipWith' f _      []     = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

triple' x = if x `mod` 3 == 0
            then True
            else False
triple x
    | x `mod` 3 == 0 = True
    | otherwise      = False

boomBangs' f xs = [ if (f x) then "BOOM! " else "BANG! " | x <- xs ]
boomBangs  f xs = map boomOrBang xs
    where boomOrBang x
              | f x       = "BOOM! "
              | otherwise = "BANG! "
-- Mind the indentation here! The last two lines must be
-- to the right of `boomOrBang'


