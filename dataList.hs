import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

--intersperse '.' "MONKEY" >>> "M.O.N.K.E.Y"

--intercalate " " ["hey", "there", "folks"] >>> "hey there folks"

sumPoly :: (Integral a) => [[a]] -> [a]
sumPoly xss = map sum $ transpose xss

search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True  else acc) False (tails haystack)
