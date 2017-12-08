{-# LANGUAGE
    GADTs,
    MultiParamTypeClasses,
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
d0 = undefined :: D0
d1 = undefined :: D1
d2 = undefined :: D2
d3 = undefined :: D3
d4 = undefined :: D4
d5 = undefined :: D5
d6 = undefined :: D6
d7 = undefined :: D7
d8 = undefined :: D8
d9 = undefined :: D9

data Vec n a where
    Nil  :: Vec Z a
    Cons :: a -> Vec n a -> Vec (S n) a

instance Show a => Show (Vec n a) where
    show vec = '[' : show' vec ++ "]"
        where show' :: Show a => Vec n a -> String
              show' Nil = ""
              show' (Cons a v) = show a ++ ","

class Natural i
instance Natural Z
instance Natural n => Natural (S n)

-- Safe head
vhead :: Vec (S n) a -> a
vhead (Cons x xs) = x

-- i < n
class WithIn n i where
    index :: Vec n a -> i -> a

instance Natural n => WithIn (S n) Z where
    index (Cons x _) _ = x

instance (Natural n, WithIn n i) => WithIn (S n) (S i) where
    index (Cons x xs) _ = index xs (undefined :: i)

intv = Cons 42 $ Cons (-8) Nil
take0 = index intv d0
take1 = index intv d1
-- Should fail
--take2 = index intv d2

-- It seems hard to add two peano numbers
--type Add n1 Z = n1

testWithIn :: (WithIn n i) => n -> i -> String
testWithIn _ _ = "success!"

tl0 = testWithIn d1 d0
tl1 = testWithIn d5 d3
tl2 = testWithIn d9 d4
-- Should fail
--tl3 = testWithIn d6 d6
--tl4 = testWithIn d0 d0
--tl5 = testWithIn d2 d8

data Only a b = Only b
instance Show b => Show (Only a b) where
    show (Only b) = show b
fromOnly :: Only a b -> b
fromOnly (Only b) = b

