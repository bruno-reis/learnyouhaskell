sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> x == y) False ys

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y = foldl (\acc x -> x == y) False

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
--map' f = foldl (\acc x -> acc ++ [f x]) [] xs