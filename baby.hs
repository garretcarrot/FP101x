doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x =
  if x > 100
    then x
    else 2 * x

doubleSmallNumber' x = (if x > 100 then x else doubleMe x) + 1

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

triangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b]]
rightTriangles = [(a,b,c) | (a,b,c) <- triangles, a^2 + b^2 == c^2]
rightTriangles' = [(a,b,c) | (a,b,c) <- rightTriangles, a + b + c == 24]
-- rightTriangles'


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

replicate' :: (Num a, Ord a) => a -> b -> [b]
replicate' n x | n <= 0 = []
replicate' n x = x : replicate' (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = 
  let smallerSorted = quicksort [y | y <- xs, y <= x]
      biggerSorted =  quicksort [y | y <- xs, y > x]
  in smallerSorted ++ [x] ++ biggerSorted