e1 = sum [n^2 | n <- [1..100]]

repl :: Int -> a -> [a]
repl n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) |
            x <- [1..n],
            y <- [1..n],
            z <- [1..n],
            x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [i | i <- [1..n], n `mod` i == 0]

perfect :: Int -> Bool
perfect n = sum (factors n) == n

perfects :: Int -> [Int]
perfects n = [i | i <- [1..n], perfect i]

e5 = concat [[(x, y) | y <- [4..6]] | x <- [1..3]]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..length xs - 1])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]

xs = 1 : [x + 1 | x <- xs]

riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

divides x y = x `mod` y == 0
divisors x = [d | d <- [1..x], x `divides` d]

