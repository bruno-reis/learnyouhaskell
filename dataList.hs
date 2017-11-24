import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

--intersperse '.' "MONKEY" >>> "M.O.N.K.E.Y"

--intercalate " " ["hey", "there", "folks"] >>> "hey there folks"

sumPoly :: (Integral a) => [[a]] -> [a]
sumPoly xss = map sum $ transpose xss

