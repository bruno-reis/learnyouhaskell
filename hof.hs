multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multPairWith9 = multThree 9

multWith18 = multPairWith9 2

compareWith100 :: (Num a, Ord a) => a -> Ordering
compareWith100 = compare 100

divideBy10 :: (Floating a) => a -> a
divideBy10 = (/10)

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])

apply2x :: (a -> a) -> a -> a
apply2x f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smaller = quicksort (filter (<= x) xs)
      bigger = quicksort (filter (> x) xs)
  in  smaller ++ [x]++ bigger

