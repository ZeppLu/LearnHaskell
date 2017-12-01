-- LYH Chapter 3. Types and Typeclasses --


-- Polymorphism in Haskell
answerToEverythingInt    = read "42" :: Int
answerToEverythingDouble = read "42" :: Double

polyMin = minBound :: Bool
polyMax = maxBound :: (Int, Char)

-- These functions (yes, a single variable can be treated as a function
-- does not accept any argument) show polymorphism on return type,
-- so it's necessary to append the `:: Type' indentifier

-- Since types cannot be cast implicitly, manually conversion is required
-- especially when dealing with numbers
average xs = sum xs / fromIntegral (length xs)
-- Rather than:
--average xs = sum xs / length xs
