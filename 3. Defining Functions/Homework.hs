import Prelude hiding ((&&))
-- import Prelude hiding ((||))

null' [] = True
null' _ = False
-- pred :: Int -> Int 
-- pred 0 = 0 
-- pred (n+1) = n

-- Slides

or' :: Bool -> Bool -> Bool
or' True _ = True
or' False x = x

or'' :: Bool -> Bool -> Bool
or'' True True = True
or'' True False = True
or'' False True = True
or'' False False = False

or''' False False = False
or''' _ _ = True

and' x y = if x then if y then True else False else False
and'' x y = if x then y else False
and''' x y  | x         = if y then True else False
            | otherwise = False

-- Parse error in pattern: head'
-- head' x:xs = x

e1 :: Num a => [[a]]
e1 = [[1, 2], [3, 4]]
e2 :: Num a => [[[a]]]
e2 = [[[1, 2, 3]], [[4, 5, 6]]]
e9 [x, y] = (x, True)
e10 (x, y) = [x, y]

-- Exercise 0

-- / expects args to be Fractional
--halve1 xs = (take n xs, drop n xs) where n = length xs / 2
halve2 xs = splitAt (length xs `div` 2) xs
halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs) where n = length xs
-- halve4 xs = splitAt (length xs `div` 2)
-- halve5 xs = (take n xs, drop (n + 1) xs) where n = length xs `div` 2
halve6 xs = splitAt (div (length xs) 2) xs
-- halve7 xs = splitAt (length xs / 2) xs
halve8 xs = (take n xs, drop n xs) where n = length xs `div` 2

-- Exercise 1

safetail0 xs = if null xs then [] else tail xs
safetail1 [] = []
safetail1 (_ : xs) = xs
-- safetail2 (_ : xs) -- nope 
--   | null xs = []
--   | otherwise = xs
safetail3 xs 
  | null xs = []
  | otherwise = tail xs
-- safetail4 xs = tail xs -- nope
-- safetail4 [] = []
safetail5 [] = []
safetail5 xs = tail xs
-- safetail6 [x] = [x] -- nope
-- safetail6 (_ : xs) = xs
safetail7
  = \ xs ->
      case xs of
        [] -> []
        (_ : xs) -> xs

-- Exercise 2

-- False || False = False
-- _ || _ = True

-- False || b = b
-- True || _ = True

-- b || c
--   | b == c = b
--   | otherwise = True

-- b || False = b
-- _ || True = True

-- Exercise 3
andTest = [True && True, False && False, True && False, False && True]

-- True && True = True
-- _ && _ = False
--a && b = if a then if b then True else False else False
--a && b = if not (a) then not (b) else True
-- a && b = if a then b
--a && b = if a then b else False
--a && b = if b then a else False
True && b = b
_ && _ = False

-- Exercise 4

mult x y z = x * y * z
mult' = \x -> (\y -> (\z -> x * y * z))

-- Exercise 7

remove n xs = take n xs ++ drop (n + 1) xs

-- Exercise 8

funct x xs = take (x + 1) xs ++ drop x xs