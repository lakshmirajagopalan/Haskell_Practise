import Data.Char

twice :: (a -> a) -> a -> a 	
twice f x = f (f x)		

quadruples :: Num a => a -> a
quadruples x = (twice (*2)) x

recMap :: (a -> a) -> [[a]] -> [[a]]
recMap f xss = map (map f) xss

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x:xs) = f x (myfoldr f v xs)

mysum :: Num a => [a] -> a
mysum xs = myfoldr (+) 0 xs

myproduct :: Num a => [a] -> a
myproduct xs = myfoldr (*) 1 xs

mylength :: [a] -> Int 
mylength xs = myfoldr (\_ l -> 1 + l) 0 xs

myreverse :: [a] -> [a]
myreverse xs = myfoldr (\y ys -> ys ++ [y]) [] xs

myfoldl :: (b -> a -> b) -> b -> [a]-> b
myfoldl f v [] = v
myfoldl f v (x:xs) =  myfoldl f (f v x) xs

myreverse' :: [a] -> [a]
myreverse' xs = myfoldl (\v x -> x:v) [] xs

type Bit = Int 
int2b :: Int -> [Bit]
int2b x = x `mod` 2 : int2b (x `div` 2)

b2int :: [Bit] -> Int
b2int (x:xs) = foldr (\x y -> x + 2*y) 0 xs 

make8 :: [Bit] -> [Bit]
make8 xs = take 8 (xs ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2b . ord)