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