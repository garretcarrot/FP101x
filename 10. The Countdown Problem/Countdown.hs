data Op = Add | Sub | Mul | Div deriving Show

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = x `mod` y == 0 && y /= 1
--
-- Used to generalize the numeric domain to arbitrary integers (Ex 11.5):
--
-- valid Add x y = True
-- valid Sub x y = True
-- valid Mul x y = True
-- valid Div x y = y /= 0 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr deriving Show

values              ::  Expr -> [Int]
values (Val n)      =   [n]
values (App _ l r)  =   values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = yss ++ map (x:) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

e1 = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, m) <- results ns', n == m]

--- Exercises ---

choices' xs = [perm | sub <- subs xs, perm <- perms sub]

removeone x [] = []
removeone x (y : ys) 
  | x == y = ys
  | otherwise = y : (removeone x ys)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x : _) [] = False
isChoice (x : xs) ys = elem x ys && isChoice xs (removeone x ys)

split' :: [a] -> [([a], [a])]
split' [] = []
split' (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split' xs]

results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [res | (ls, rs) <- split' ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e, m) <- results' ns', n == m]

pool :: [Int]
pool = [1, 3, 7, 10, 25, 50]
