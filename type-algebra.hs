-- http://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

-- single value type, also known as ()
data Unit = Unit deriving (Eq, Show)

-- zero value type, unconstructible type in other words
data Void

-- sum type
data Add a b = AddL a | AddR b deriving (Eq, Show)

-- product type
data Mul a b = Mul a b

-- equality on types
-- should obey two rules:
--   o back . fore == (id :: a -> a)
--   o fore . back == (id :: b -> b)
-- TODO: add laws using singletons and other teckniques
class EqualTypes a b where
    fore :: a -> b
    back :: b -> a

-- to write is easy, but how to prove?
instance EqualTypes (Mul a Unit) a where
    fore (Mul x _) = x
    back x = (Mul x Unit)

