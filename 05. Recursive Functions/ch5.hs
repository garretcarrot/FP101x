import Prelude hiding (
       product
     , length
     , reverse
     , zip
     , drop
     , (++)
     , and
     , concat
     , replicate
     , (!!)
     , elem
     , (^)
     , sum
     , take
     , last
     )

factorial 0 = 1
factorial n = n * factorial (n - 1)

product [] = 1
product (x : xs) = x * product xs

length [] = 0
length (_ : xs) = 1 + length xs

reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_:xs) = drop (n - 1) xs

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x:xs) ++ ys = x : xs ++ ys

qsort [] = []
qsort (x : xs) =
  qsort lesser ++ [x] ++ qsort greater
  where
    lesser = [x' | x' <- xs, x' <= x]
    greater = [x' | x' <- xs, x' > x]

and []          = True
and (False : _) = False
and (True : xs) = and xs

concat :: [[a]] -> [a]
concat [] = []
concat (xs : xss) = xs ++ concat xss

replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

elem _ [] = False
--elem x' (x : xs) = x == x' || elem x' xs
elem x (y : ys)
  | x == y = True
  | otherwise = elem x ys

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

msort [] = []
msort [x] = [x]
msort xs =
  merge (msort a) (msort b)
  where (a, b) = halve xs

halve xs = splitAt (length xs `div` 2) xs

factorial' n = product (to n)
product' [] = 1
product' (x:xs) = x * (product xs)
to 1 = [1]
to n = (to (n - 1)) ++ [n]

0 ^ _ = 0
_ ^ 0 = 1
n ^ m = n * (n ^ (m - 1))

-- init [1, 2, 3]
-- 1 : init [2, 3]
-- 1 : 2 : init [3]
-- 1 : 2 : []
-- [1, 2]

take 0 _ = []
take _ [] = []
take n (x : xs) = x : take (n - 1) xs

sum [] = 0
sum (x : xs) = x + sum xs

last [x] = x
last (x : xs) = last xs

and :: [Bool] -> Bool

andTests = [[True, True, False], [False, False, False], [True, True, True]]
andTest = [and xs | xs <- andTests]

--and [] = True
--and (b : bs) = b && and bs

--and [] = True
--and (b : bs)
--  | b = and bs
--  | otherwise = False

-- nope
-- and [] = False
-- and (b : bs) = b && and bs

-- nope
-- and [] = False
-- and (b : bs) = b || and bs

-- and [] = True
-- and (b : bs)
--   | b == False = False
--   | otherwise = and bs

--nope
--and [] = True
--and (b : bs) = b || and bs

-- and [] = True
-- and (b : bs) = and bs && b

-- nope
-- and [] = True
-- and (b : bs)
--   | b = b
--   | otherwise = and bs
