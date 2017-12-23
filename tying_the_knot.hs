{-# LANGUAGE
    StandaloneDeriving,
    GADTs
#-}

-- https://wiki.haskell.org/Tying_the_Knot

data DList a where
    DLNode :: Eq a => DList a -> a -> DList a -> DList a
deriving instance Eq a => Eq (DList a)

mkDList :: Eq a => [a] -> DList a

mkDList [] = error "empty list"
mkDList xs = first
    -- Magic goes here
    where (first, last) = go last xs first
          go :: Eq a => DList a -> [a] -> DList a -> (DList a, DList a)
          go prev []     next = (next, prev)
          -- Contruct a segment of nodes
          go prev (x:xs) next = (this, last)
              where this         = DLNode prev x rest
                    (rest, last) = go this xs next

takeF :: Integral b => b -> DList a -> [a]
takeF 0 _              = []
takeF n (DLNode _ x f) = x : takeF (n-1) f

takeB :: Integral b => b -> DList a -> [a]
takeB 0 _              = []
takeB n (DLNode b x _) = x : takeB (n-1) b

dList = mkDList "Hello, world!"
naiveDList = x
    where x = 0 : y
          y = 1 : x
-- Try to do either `dList == dList' or `naiveDList == naiveDList',
-- spy their memory usage, figure out where the difference comes from


-- From https://stackoverflow.com/a/9732857/8943081
-- Eq is required cause label act as node's ID
data Node a where
    Node :: Eq a => {
        label    :: a
      , adjacent :: [Node a]
      } -> Node a
-- Default `show' would go on forever
--deriving instance Show a => Show (Node a)
instance (Show a) => Show (Node a) where
    show (Node lbl adj) = show labels
        where labels = lbl : map label adj
-- Comparing for equality seems unachievable,
-- cause Node tends to expand into an infinite tree
--deriving instance Eq   a => Eq   (Node a)

infiniteNode = node
    where node  = Node 42   [node']
          node' = Node (-8) [node]

data Graph a where
    Graph :: Eq a => {
        nodes :: [Node a]
    } -> Graph a
deriving instance Show a => Show (Graph a)

mkGraph :: Eq a => [(a, [a])] -> Graph a
mkGraph vertices = Graph $ map snd nodes
    where nodes = map mkNode vertices
          mkNode (label, ns) = (label, Node label $ map lookupNode ns)
          lookupNode label = removeJust $ lookup label nodes
          removeJust (Just a) = a

--extractNodes :: Graph a -> [Node a]
--extractNodes (Graph ns) = ns

--   6---4---5--.
--       |   |   \
--       |   |    1
--       |   |   /
--       3---2--'
vertices = [(1,[2,5]),(2,[1,3,5]),(3,[2,4]),(4,[3,5,6]),(5,[1,2,4]),(6,[4])]
graph = mkGraph vertices
