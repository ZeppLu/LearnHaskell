-- LYH Chapter 7. Modules --


import Data.List
import Data.Function (on)

-- More list-related functions
-- `intersperse', `intercalate', `transpose', `concat'
join xs xss = concat (intersperse xs xss)
join' = intercalate

-- `iterate'
twoPowers = iterate (*2) 1

-- `sort', `group', `groupBy', `nub', `nubBy'
-- A codewars kata
uniqueInOrder :: (Ord a, Eq a) => [a] -> [a]
uniqueInOrder = sort . nub
uniqueInOrder' :: (Ord a, Eq a) => [a] -> [a]
uniqueInOrder' = nub . sort
uniqueInOrder'' :: (Ord a, Eq a) => [a] -> [a]
uniqueInOrder'' = map head . group . sort

-- `inits', `tails'

-- `find' (-> Maybe a), `findIndex' (-> Maybe a), `findIndices' (-> [a])

-- `isPrefixOf', `isInfixOf', `isSuffixOf'

-- `elemIndex' (-> Maybe a), `elemIndices' (-> [a])

-- `delete', `deleteBy' `(\\)'

-- `union', `unionBy', `intersect', `intersectBy'
-- `insert'

-- `generic{Length,Take,Drop,SplitAt,Index,Replicate}' and
--          length,take,drop,splitAt,(!!), replicate

-- `on' from Data.Function
sameSign :: (Num a, Ord a) => a -> a -> Bool
sameSign = (==) `on` (compare 0)
sameSign' :: (Num a, Ord a) => a -> a -> Bool
sameSign' = (==) `on` signum

-- `sortBy', `insertBy', `maximumBy', `minimumBy'

-- NEXT: Data.Char
