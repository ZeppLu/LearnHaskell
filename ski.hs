-- TODO: add codewars link

{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}

data SKI :: * -> * where
    S  :: SKI ((a -> b -> c) -> (a -> b) -> a -> c)
    K  :: SKI (a -> b -> a)
    I  :: SKI (a -> a)
    Ap :: SKI (a -> b) -> SKI (a) -> SKI (b)

-- a convenient symbol to avoid tons of parentheses
infixl <|
(<|) = Ap

instance Show (SKI c) where
    show S        = "S"
    show K        = "K"
    show I        = "I"
    show (Ap a b) = "(" ++ show a ++ " " ++ show b ++ ")"

-- equality is guaranteed by type system
-- you can check this by `Ap (Ap S K) _ == I`
instance Eq (SKI c) where
    a == b = True

data Variables a = Sing a
                 | Cons a (Variables a)

--data Lambda :: Variables * -> * where
--    Var :: a -> Lambda (Sing a)
--    Abs :: a -> Lambda b -> Lambda (Just a)
--    App :: Lambda a -> Lambda b -> Lambda c
--
--instance (Show a) => Show (Lambda (Maybe a)) where
--    show (Var a) = show a
--    show (Abs x y) = "(λ" ++ show x ++ ". " ++ show y++ ")"
--    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"
