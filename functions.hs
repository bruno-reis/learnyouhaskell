lucky :: (Integral a) => a -> String
lucky 7 = "Lucky #7 !!!"
lucky x = "Sorry, try again!"

fac :: (Integral a ) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell w h
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "overweight"
  | otherwise   = "obese"
  where bmi = w / h ^ 2

