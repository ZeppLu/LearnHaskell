{-# LANGUAGE RankNTypes #-}

-- https://wiki.haskell.org/Curry-Howard-Lambek_correspondence
-- https://en.wikibooks.org/wiki/Haskell/The_Curry–Howard_isomorphis

data Void

empty :: Void -> a
empty = empty

-- Is it legal?
toVoid :: a -> Void
toVoid = toVoid

type Not a = a -> Void
type And a b = (a, b)
type Or a b = Either a b

doubleNeg :: x -> Not (Not x)
doubleNeg k pr = pr k

-- A & ~A
type Contradiction a = And a (Not a)

-- A | ~A
type ExcludedMiddle a = Or a (Not a)

type Not' x = (forall a. x -> a)
doubleNeg' :: x -> Not' (Not' x)
doubleNeg' k pr = pr k
