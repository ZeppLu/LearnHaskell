-- LYH Chapter 5. Recursion --

qSort [] = []
qSort (x:xs) = sortedSmaller ++ [x] ++ sortedLarger
    where sortedSmaller = qSort $ filter (<=x) xs
          sortedLarger  = qSort $ filter (> x) xs
-- Using list comprehension is also fine, but less readable


