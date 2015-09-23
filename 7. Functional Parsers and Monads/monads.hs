data Expr = Val Int | Div Expr Expr

-- (>>=)   :: Maybe a -> (a -> Maybe b) -> Maybe b
-- m >>= f =  case m of
--              Nothing -> Nothing
--              Just x  -> f x

safediv :: Int -> Int -> Maybe Int
safediv n 0 = Nothing
safediv n m = Just (n `div` m)

eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    safediv n m

eval' :: Expr -> Maybe Int
eval' (Val n)   = Just n
eval' (Div x y) = case eval' x of
                      Nothing -> Nothing
                      Just n  -> case eval' y of
                                    Nothing -> Nothing
                                    Just m -> safediv n m



seqn'                    :: Maybe a -> Maybe b -> Maybe (a,b)
seqn' Nothing   _        =  Nothing
seqn' _         Nothing  =  Nothing
seqn' (Just x)  (Just y) =  Just (x,y)

seqn :: Maybe a -> Maybe b -> Maybe (a,b)
seqn x y = do n <- x
              m <- y
              Just (n, m)

-- eval (Op x y z) = do a <- x
--                      b <- y
--                      c <- z
--                      (eval a `seqn` (eval b `seqn` eval c))
