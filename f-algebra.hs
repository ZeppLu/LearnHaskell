-- https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/

--{-# LANGUAGE TypeSynonymInstances #-}
--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


-- try to abstract recursion away from recursive data type
-- frequently seen recursive form:
-- data List a = Nil | Cons a (List a)
data ListF a l = Nil
               | Cons { lhead :: a
                      , ltail :: l
                      }
                 deriving (Show, Eq)
-- ListF :: (* -> *) -> (* -> *)  -- NOTE that this is a type function

-- now let's rock, here comes abstracted recursion: f(fix(f)) = fix(f)
-- Fix :: (* -> *) -> *
-- Fix :: f (Fix f) -> Fix f
newtype Fix f = Fix { unFix :: f (Fix f) }
-- equivalent notation (used in some literatures):
-- newtype Mu f = InF { outF :: f (Mu f) }
instance Show (f (Fix f)) => Show (Fix f) where
    show (Fix x) = "(" ++ show x ++ ")"
instance Eq (f (Fix f)) => Eq (Fix f) where
    (Fix x) == (Fix y) = x == y

-- List a = Fix (ListF (Fix ListF) a)
type List a = Fix (ListF a)

instance Functor (ListF a) where
    fmap _ (Nil      ) = Nil
    fmap f (Cons x xs) = Cons x $ f xs

{-
 - An F-algebra consists of:
 - o an endofunctor F in a catagory C,
 - o an object A in that category, and
 - o a morphism from F(A) to A.
 -}

-- related to both functor and carrier type
-- Fix :: Algebra f (Fix f)
-- Fix :: Algebra (ListF a) (List a)
type Algebra f a = f a -> a

{-
 -             fmap g
 -  f (Fix f) -------> f a
 -     | ^              |
 -   In| |unFix         |algebra
 -     v |              v
 -    Fix f ----------> a
 -               g
 -}

testList :: Num a => List a
testList = Fix $ Cons 42 $
                      Fix $ Cons (-8) $
                                 Fix $ Cons 0 $
                                            Fix Nil
shouldNil = tail' $ tail' $ tail' $ testList
shouldFail = tail' $ tail' $ tail' $ tail' $ testList

-- head & tail
head' :: List a -> a
head' = lhead . unFix
tail' :: List a -> List a
tail' = ltail . unFix
