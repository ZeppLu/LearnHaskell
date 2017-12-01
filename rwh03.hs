-- RWH Chapter 3. Defining Types, Streamlining Functions --


-- ch03/BookStore.hs, ch03/AltCustomer.hs
data Customer = Customer {
      customerID      :: Int
    , customerName    :: String
    , customerAddress :: [String]
    } deriving (Show)
-- So that you can bound value in your favorite C99 style
customerExample = Customer {
      customerName    = "Zepp"
    , customerID      = 678
    , customerAddress = [
          "Tsinghua Park #1",
          "Haidian",
          "Beijing",
          "China"
      ]
    }
-- Members can be extracted like this
myName = customerName customerExample

-- RWH ch03 ListADT.hs
data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Convert between List and []
fromList Nil         = []
fromList (Cons x xs) = x : fromList xs
toList []     = Nil
toList (x:xs) = Cons x (toList xs)

-- RWH ch03 Tree.hs
data NullableTree a = NullableTree a (Maybe (NullableTree a)) (Maybe (NullableTree a))
              deriving (Show)
-- Mind the parenthesis!
-- Example: NullableTree 42 Nothing (Just (NullableTree 42 Nothing Nothing))

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- How to correctly use pattern matching
-- Failed. Cause a name can appear only once in a pattern (except _)
--nodesAreSame' (Node a _ _) (Node a _ _) = True
--nodesAreSame' _            _            = False
-- Good pratice: using guards
nodesAreSame (Node a _ _) (Node b _ _)
    | a == b = True
nodesAreSame _ _ = False


-- RWH ch03 ex07
-- Similar to join in python
intersperse _ []     = []
intersperse _ [x]    = x
intersperse j (x:xs) = x ++ j ++ intersperse j xs

-- RWH ch03 ex08
treeHeight Empty        = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- RWH ch03 ex09
data Direction = GoStraight
               | TurnLeft
               | TurnRight
                 deriving (Show, Eq)

-- Failed if I put '(Num a) =>' here
-- Cause: Haskell 98 disallow constraints on constructors
-- Solution: use GADT (see below) 
--data Point1 a = Point1 a a deriving (Show, Eq, Ord)
-- GADT, requires flag -XGADTs
data Point a where
      Point :: (Num a, Ord a) => {
          x :: a
        , y :: a
      } -> Point a
-- requires flag -XStandaloneDeriving
deriving instance Show a => Show (Point a)
deriving instance Eq   a => Eq   (Point a)

-- RWH ch03 ex10
-- Another simpler approach: by cross product
calcDirection p1@(Point x1 y1) p2@(Point x2 y2) p3@(Point x3 y3)
    | p1 == p2 = GoStraight
    | p2 == p3 = GoStraight
    | p1 == p3 = GoStraight
    | (y3-y2)*(x2-x1) == (y2-y1)*(x3-x2) = GoStraight
    | otherwise = if dotProd (rotate $ vector p1 p2) (vector p2 p3) > 0
                  then TurnLeft
                  else TurnRight
    where vector (Point x1 y1) (Point x2 y2) = Point (x2 - x1) (y2 - y1)
          rotate (Point x y) = Point (-y) x
          dotProd (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

-- RWH ch03 ex11
calcDirections (p1:p2:p3:ps) = calcDirection p1 p2 p3 : calcDirections (p2:p3:ps)
calcDirections _ = []

-- NEXT: ex12

