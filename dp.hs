{-# LANGUAGE
    DataKinds,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances
#-}

-- https://www.schoolofhaskell.com/user/konn/prove-your-haskell-for-great-safety/dependent-types-in-haskell
-- https://zhuanlan.zhihu.com/p/31690842 (在Haskell中模拟dependent type)

data Nat = Z | S Nat

infixl 6 :+
type family (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

infixl 7 :*
type family (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

--data Vec n a where
--    Nil  :: Vec Z a
--    Cons :: a -> Vec n a -> Vec (S n) a
