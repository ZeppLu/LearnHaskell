import Data.Ratio
import Data.List

data Digit = Neg
           | Zero
           | One
           | Two
           | Three
           | Four
           | Five
           | Six
           | Seven
           | Eight
           | Nine
             deriving (Show, Eq, Ord, Enum)

instance Num Digit where
    fromInteger x
        | x == 0           = Zero
        | x >= 1 && x <= 9 = succ $ fromInteger (x-1)
    (+) x y = fromInteger (s `mod` 10)
        where s = toInteger x + toInteger y
    (-) x y = fromInteger (r `mod` 10)
        where r = toInteger x - toInteger y
    (*) x y = fromInteger (p `mod` 10)
        where p = toInteger x * toInteger y
    abs = id
    signum Zero = Zero
    signum _    = One

instance Real Digit where
    toRational Zero = 0 % 1
    toRational x    = 1 + toRational (pred x)

instance Integral Digit where
    toInteger Zero = 0
    toInteger x    = 1 + toInteger (pred x)
    quotRem x y = (fromInteger a, fromInteger b)
        where (a, b) = quotRem (toInteger x) (toInteger y)

carryOrLend :: (Integer -> Integer -> Integer) -> Digit -> Digit -> Integer
carryOrLend f x y
    | result < 0   = -1
    | result >= 10 = quot result 10
    | otherwise    = 0
          where result = f (toInteger x) (toInteger y)


join :: [a] -> [[a]] -> [a]
join _  []  = []
join xs xss = foldl1 joinNew xss
    where joinNew acc ys = acc ++ xs ++ ys

data Expr = Cst Int
          | Var
          | Add [Expr]
          | Mul [Expr]
instance Show Expr where
    show (Cst c) = show c
    show Var = "x"
    show (Mul xs) = "(" ++ join "*" (map show xs) ++ ")"
    show (Add xs) = "(" ++ join "+" (map show xs) ++ ")"
data Equal = Equal Expr Expr

--normalize :: Expr -> Expr
--normalize (Cst x) = Cst x
--normalize Var = Var
--normalize (Mul (Add x y) z) = Add (normalize (Mul x z)) (normalize (Mul y z))
--normalize (Mul z (Add x y)) = normalize (Mul (Add x y) z)
--normalize (Mul (Cst x) (Cst y)) = Cst (x * y)
--normalize (Mul x y) = Mul (normalize x) (normalize y)
--normalize (Add x y) = Add (normalize x) (normalize y)

{-
normalize :: Expr -> Expr
normalize (Add xs) = merge $

testExpr = Mul [Add [Var, Cst 2], Add [Mul [Cst 2, Var], Cst 3]]

solve :: Equal -> Maybe Int
solve (Equal    (Cst _) (Cst _)) = Nothing
solve (Equal    (Cst c) all@_  ) = solve (Equal all (Cst c))
solve (Equal    Var     (Cst c)) = Just c
-}

