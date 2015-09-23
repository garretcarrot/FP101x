import Data.List
import Data.Char
-- import Unsafe.Coerce

data Nat
  = Zero
  | Succ Nat
  deriving Show

natToInteger :: Nat -> Integer
natToInteger (Succ n) = natToInteger n + 1
natToInteger Zero = 0
-- natToInteger = \n -> length [c | c <- show n, c == 'S']
-- natToInteger = head . m
--   where m Zero = [0]
--         m (Succ n) = [sum [x | x <- (1 : m n)]]
-- natToInteger n = natToInteger n

integerToNat :: Integer -> Nat
integerToNat (n + 1) = let m = (integerToNat n) in Succ m
integerToNat 0 = Zero
-- integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

two = Succ (Succ Zero)
three = Succ two

mult :: Nat -> Nat -> Nat
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)

data Tree = Leaf Integer
          | Node Tree Integer Tree

testTree = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs :: Integer -> Tree -> Bool
occurs m (Leaf n) = m == n
-- occurs m (Node l n r)
--   = case compare m n of 
--       LT -> occurs m r
--       EQ -> True
--       GT -> occurs m l
occurs m (Node l n r)
  | m == n    = True
  | m > n     = occurs m l
  | otherwise = occurs m r

tsize :: Tree -> Int
tsize (Leaf _) = 1
tsize (Node t1 _ t2) = 1 + tsize t1 + tsize t2

complete :: Tree -> Bool
complete (Leaf _) = True
complete (Node t1 _ t2) = (tsize t1) == (tsize t2)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr 

e = Add (Val 1) (Mul (Val 2) (Val 3))

size :: Expr -> Int
size (Val n) = 1
size (Add e1 e2) = size e1 + size e2
size (Mul e1 e2) = size e1 + size e2

eval :: Expr -> Int
eval (Val n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

fold :: (Int -> Int) -> (Int -> Int -> Int) -> (Int -> Int -> Int) -> Expr -> Int
fold vf af mf (Val n) = vf n
fold vf af mf (Add e1 e2) = (af (fold vf af mf e1) (fold vf af mf e2))
fold vf af mf (Mul e1 e2) = (mf (fold vf af mf e1) (fold vf af mf e2))

size' = fold (const 1) (+) (+)
eval' = fold (id) (+) (*)


