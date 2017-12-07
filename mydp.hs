{-# LANGUAGE
    GADTs,
    MultiParamTypeClasses,
    FlexibleInstances,
    ScopedTypeVariables
#-}

data Z
data S n
type D0 = Z
type D1 = S D0
type D2 = S D1
type D3 = S D2
type D4 = S D3
type D5 = S D4
type D6 = S D5
type D7 = S D6
type D8 = S D7
type D9 = S D8

data Vec n a where
    Nil  :: Vec Z a
    Cons :: a -> Vec n a -> Vec (S n) a

instance Show a => Show (Vec n a) where
    show vec = '[' : show' vec ++ "]"
        where show' :: Show a => Vec n a -> String
              show' Nil = ""
              show' (Cons a v) = show a ++ ","

data Only a b = Only b
instance Show b => Show (Only a b) where
    show (Only b) = show b
fromOnly :: Only a b -> b
fromOnly (Only b) = b

class Natural i
instance Natural Z
instance Natural n => Natural (S n)

-- Safe head
vhead :: Vec (S n) a -> a
vhead (Cons x xs) = x

-- m < n
class LessThan n i where
    index :: Vec n a -> Only i a

instance Natural n => LessThan (S n) Z where
    index (Cons x _) = Only x

instance (Natural n, LessThan n i) => LessThan (S n) (S i) where
    index (Cons (x::a) xs) = Only extracted
        where (Only extracted) = index xs :: Only i a

intv = Cons 42 $ Cons (-8) Nil
take0 = fromOnly (index intv :: Only D0 Int)
take1 = fromOnly (index intv :: Only D1 Int)
-- Should failed
--take2 = fromOnly (index intv :: Only D2 Int)


testLess :: (LessThan n m) => Only (n, m) String
testLess = Only "success!"

tl0 = testLess :: Only (D1, D0) String
tl1 = testLess :: Only (D5, D3) String
tl2 = testLess :: Only (D9, D4) String
-- Should failed
--tl3 = testLess :: Only (D6, D6) String
--tl4 = testLess :: Only (D0, D0) String
--tl5 = testLess :: Only (D2, D8) String


-- m x n matrix

type Matrix m n a = Vec m (Vec n a)
