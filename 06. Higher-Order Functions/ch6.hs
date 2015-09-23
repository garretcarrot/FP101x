import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
-- bin2int bits = sum [w * b | (w, b) <- zip weights bits]
--   where weights = iterate (*2) 1
bin2int = foldr (\ x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = (n `mod` 2) : int2bin (n `div` 2)

make9 :: [Bit] -> [Bit]
make9 bits = (parity bs) : bs
  where bs = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity = ((`mod`2) . (sum . (filter (==1))))

encode :: String -> [Bit]
encode = concat . map (make9 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits
  | (head bs) == (parity (tail bs)) = tail bs : chop9 (drop 9 bits)
  | otherwise                       = error "parity error"
      where bs = take 9 bits

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail

-- 



-- Exercises

comprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension f p = (map f) . (filter p)

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldl (\ acc elem -> acc && p elem) True

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldl (\ acc elem -> acc || p elem) False

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x : xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x : xs)
  | p x = dropWhile' p xs
  | otherwise = (x : xs)

map' :: (a -> b) -> [a] -> [b]
-- 1 : (2 : (3 : []))
-- f 1 : (f 2 : (f 3 : []))
map' f = foldr (\x xs -> (f x) : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if (p x) then x : xs else xs) []

dec2int = foldl (\b a -> 10 * b + a) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

sumsqr (x, y) = x^2 + y^2
sumsqr' = curry' sumsqr

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

sumsqr2 = uncurry' sumsqr'

unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (==0) (`mod`2) (`div`2)

chop8' = unfold null (take 8) (drop 8)

map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (\_ -> False) id f

-- wrong
dropWhile'' p = foldl add [] 
  where add [] x  = if p x then [] else [x]
        add acc x = x : acc



