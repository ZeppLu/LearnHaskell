-- LYH Chapter 4. Syntax in Functions --


-- While pattern matching, an `@' can by used to replace a whole
strong []         = "!!!"
strong all@(x:xs) = take 3 (repeat x) ++ all ++ "!"

-- Always remember that `let .. in ..' is an expression,
-- which can be written wherever expressions are legal.
-- While `where' is bound to a surrounding syntactic contruct.

-- Usage of guard and `where'
bmiTell weight height
    | bmi <= skinny = "Too thin"
    | bmi <= normal = "Fair"
    | bmi <= fat    = "Delicious"
    | otherwise     = "BOOM!"
    where bmi = weight / height^2
          -- Neat trick
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Normally `where' is used like above
-- `let' is more frequently seen in list/tuple comprehension
calcFatBmis xs = [bmi | (w, h) <- xs, let bmi = w / h^2, bmi > 30]

-- For more info, read https://wiki.haskell.org/Let_vs._Where

-- Partial pattern matching
howLong xs = "This list is " ++ long xs ++ " long"
    where long []     = "zero"
          long [_]    = "one"
          long [_, _] = "two"
          long _      = "too"
-- Since `case' is an expression, doing so is also fine,
-- despite its less readability
howLong' xs = "This list is " ++
                  case xs of
                      []     -> "zero"
                      [_]    -> "one"
                      [_, _] -> "two"
                      _      -> "too"
                  ++ " long"
