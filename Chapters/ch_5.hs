import Data.Char

mylength :: [a] -> Int
mylength xs = sum[ 1 |_ <- xs]

myconcat :: [[a]] -> [a]
myconcat xss = [ x | xs <- xss, x <- xs]

myfirsts :: [(a, b)] -> [a]
myfirsts ps = [ x | (x, _) <- ps]  

myfactors :: Int -> [Int]
myfactors n = [ x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = myfactors n == [1,n]

primes :: Int -> [Int]
primes n = [ x | x <- [2..n], prime x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs] 

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ i | (x', i) <- zip xs [0..(length xs - 1)], x == x']

lowers :: String -> Int
lowers xs = sum[ 1 | x <- xs, isLower x]

count ::  Char -> String -> Int
count x xs = sum[ 1 | x' <- xs, x' == x]

countrec :: Char -> String -> Int
countrec y [] = 0
countrec y (x:xs)| y == x = countrec y xs + 1
			  | otherwise = countrec y xs

counttailrec :: Int -> Char -> String -> Int
counttailrec acc y [] = acc
counttailrec acc y (x:xs) | y == x = counttailrec (acc + 1) y xs
							|otherwise = counttailrec acc y xs
			  

int2let :: Int -> Char
int2let n = chr(n + ord 'a')

let2int :: Char -> Int
let2int c = ord c - ord 'a'

shift :: Int -> Char -> Char
shift n x | isLower x = int2let((let2int x + n) `mod` 26)
		  | otherwise = x	

encode :: Int -> String -> String
encode n xs = [ shift n x | x <- xs]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n/ fromIntegral m) * 100

freqs :: String -> [Float]
freqs xs = [ percent (count x xs) n | x <- ['a'..'z']]
			where n = lowers(xs)

sumofsquares :: Int	-> Int
sumofsquares n = sum[ x * x| x <- [1..n]]


replicaterec :: Int -> a -> [a]
replicaterec 0 a = []
replicaterec n a = a : (replicaterec (n - 1) a)
				
sum' acc [] = acc
sum' acc (x:xs) = sum' (x+acc) xs 

fact 0 = 1
fact n  = n * fact (n-1)

fact' 0 acc = acc
fact' n acc = fact' (n-1) (n * acc)

fib :: Int -> Int
fib 1 = 0
fib 2 = 1
fib n = fib (n-1) + (fib(n-2))

fibtailrec a b 1 = b
fibtailrec a b n =  fibtailrec b (a+b) (n-1)

even' n = n : even' (n+2)


fibinf = fib' 0 1 where
				   fib' a b = (a+b) : fib' b (a+b)

factors :: Int -> [Int]
factors n x <- [1..n/2]

perfect :: Int -> [Int]
perfect n = 				   



