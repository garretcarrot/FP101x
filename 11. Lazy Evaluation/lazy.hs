sumwith :: Int -> [Int] -> Int
sumwith v [] = v
sumwith v (x : xs) = (sumwith $! (v + x)) xs

-- mult = λx → (λy → x ∗y)
-- 
-- mult 3 4
-- (λx → (λy → x ∗ y)) 3 4
-- (λy → 3 * y) 4
-- 3 * 4
-- 12

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

fib :: Int -> Integer
fib = (fibs !!)

-- head (dropWhile (< 1000) fibs)

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeatTree :: a -> Tree a
repeatTree x = Node (repeatTree x) x (repeatTree x)