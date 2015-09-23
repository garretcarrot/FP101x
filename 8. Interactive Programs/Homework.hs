putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn' :: String -> IO ()
putStrLn' [] = putChar '\n'
-- putStrLn' [] = return ""
-- putStrLn' xs = putStr' xs >> putStrLn' ""
-- putStrLn' xs = putStr' xs >> putChar '\n'
-- putStrLn' xs = putStr' xs >>= \x -> putChar '\n'
-- putStrLn' xs = putStr' xs >> \x -> putChar '\n'
-- putStrLn' xs = putStr' xs >> putStr' "\n"
-- putStrLn' xs = putStr' xs >> putStrLn' "\n"
-- putStrLn' xs = putStrLn' xs >> putStr' "\n"

-- sequence_' :: Monad m => [m a] -> m ()
-- sequence_' ms = foldr (>>) (return []) ms

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m : ms) 
  = do a <- m
       as <- sequence' ms
       return (a : as)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f acc [] = return acc
foldLeftM f acc (b : bs)
  = do newAcc <- (f acc b)
       foldLeftM f newAcc bs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f acc [] = return acc
foldRightM f acc (elem : elems)
  = do res <- (foldRightM f acc elems)
       f elem res

liftM :: Monad m => (a -> b) -> m a -> m b
-- liftM f m = do {x <- m; return (f x)}
-- liftM f m = m >>= \a -> return (f a)
-- liftM f m = return (f m)
-- liftM f m = m >>= \a -> m >>= \b -> return (f b)
-- liftM f m = mapM f [m]
liftM f m = m >> \a -> return (f a)