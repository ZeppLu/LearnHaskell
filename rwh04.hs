-- RWH Chapter 4. Functional programming --

import Control.Exception (assert)

-- Better to use `null' than `length', because of the lazy evaluation
-- Or, pattern matching is also a good idea (which actually resembles
-- what `null' does, so less readable)
-- This sucks on infinite lists, also decrease performance on long lists
safeHeadDumb xs = if length xs > 0
                  then Just $ head xs
                  else Nothing
safeHead xs
    | null xs   = Nothing
    | otherwise = Just $ head xs
safeHead' [] = Nothing
safeHead' xs = Just $ head xs

-- Partial / total functions (think of `totality' in Idris)

-- `and', `or', `all', `any' on empty list
-- Be careful! Assertion is done when you need its value!
andEmpty = assert (and      []) True
orEmpty  = assert (not $ or []) False

-- Usefull functions dealing with strings
cutByLine = lines "multi\nline\ntext"
concatByLine = unlines ["multi", "line", "text"]
cutBySpace = words "Can  \ryou\nhear\t me,\r\n  Alice?"
concatBySpace = unwords ["Can", "you", "hear", "me,", "Alice?"]
-- Furthermore:
-- `take', `drop' and `takeWhile', `dropWhile'
-- `splitAt' "tuples up" the results of `take' and `drop'
-- `span' "tuples up" those of `takeWhile'
-- `break' "tuples up" those of `dropWhile'

-- ex02, both work
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
--splitWith p all@(x:xs) = case break p all of
--    ([]   , _   ) -> splitWith p xs
--    (first, rest) -> first : splitWith p rest
-- `dropWhile null' to remove null list at the head brought by `break'
splitWith p (x:xs) = dropWhile null $ first : restSplit
    where (first, rest) = break p (x:xs)
          restSplit = splitWith p $ dropWhile p rest


-- Failed to get a reasonably working `zipWithN'
-- Further readings:
-- * http://okmij.org/ftp/Haskell/polyvariadic.html
-- * https://stackoverflow.com/questions/20558648/what-is-the-datakinds-extension-of-haskell
-- * https://www.reddit.com/r/haskell/comments/b9qyp/generalized_zipwithn_with_a_pretty_implementation/

{-
--data Z
--data S n

class NatureNum a where
    fromNum :: Integral b => b -> a
    toNum   :: Integral b => a -> b

--data Nat n where
--    Zero :: Nat Z
--    Succ :: Nat n -> Nat (S n)
data Nat = Z | S Nat

--instance NatureNum (Nat) where
--    toNum  Z    = 0
--    toNum (S n) = 1 + toNum n

--instance NatureNum Zero where
--    fromNum _ = Zero
--    toNum   _ = 0
--instance NatureNum (Succ n) where
--    fromNum _ = Succ Zero
--    toNum   _ = 1

--fromNum :: Integral a => a -> (Nat n)
--fromNum 0 = Zero
----fromNum k = Succ ( fromNum (k-1))
--fromNum 1 = Succ Zero


--data NArgsFunc a :: Nat -> * where
--    NArgsFunc :: 

--data NArgsFunc a = ZeroArgFunc a
--                 | NArgsFunc a (NArgsFunc a)

type ZeroArgFunc a = a
testZ :: (ZeroArgFunc Int)
testZ = (42)
type NArgsFunc Nat a = a -> (NArgsFunc Z 
test1 :: (NArgsFunc [Char] (ZeroArgFunc Int))
test1 = length
-}
