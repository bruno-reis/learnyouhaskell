doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmall x = if x > 100
  then x
  else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..c], a^2 + b^2 == c^2 ]

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..c], a^2 + b^2 == c^2, a+b+c == 24 ]