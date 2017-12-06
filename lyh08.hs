-- LYH Chapter 8. Making Our Own Types and Typeclasses --



-- Adding typeclass constraint to data declaration is not recommended.
-- Because anyway you'd end up also having to add it to function signature.
-- data (Ord k) => Map k v = ...

-- class Functor (f :: * -> *) where ...
-- f has kind `* -> *', such as `Either a', `[]', `Maybe', `->'
fmapEither = fmap (==0) (Right 42)
fmapTuple = fmap (==0) ("something", 42)
fmapFunction = fmap (==0) (+4) $ (-4)
-- Functors can be treated as types acting like a box.
-- A type constructor (or partially applied, since currying)

-- Be aware! Function types like `Int -> Bool' are still of `*' kind.
-- Any type of existing value must be of `*' kind

-- Try to figure out what kind t, j, a is of
class Tofu t where
    tofu :: j a -> t a j

