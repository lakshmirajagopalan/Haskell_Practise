myabs :: Int -> Int
myabs n = if n >= 0 then n else -n

mysignum :: Int -> Int
mysignum n = if n < 0 then -1 else 
			 if n == 0 then 0 else 1

absgaurd :: Int -> Int
absgaurd n | n>=0 = n
			| True = 0
			| otherwise = -n	

signumguard :: Int -> Int
signumguard n | n >0 = 1
			| n == 0 = 0
			| otherwise = -1			

mynot :: Bool -> Bool
mynot False = True
mynot True = False

myand :: Bool -> Bool -> Bool
myand True b = b
myand _ _  = False					 

myandcond :: Bool -> Bool -> Bool
myandcond x y = if x == True then if y == True then True else False else False

myandcond2 :: Bool -> Bool -> Bool
myandcond2 x y = if x == True then y else False

test :: [Char] -> Bool
test ['a', _, _] = True
test _ = False

testcons :: [Char] -> Bool
testcons ('a':_) = True
testcons _ = False

mynull :: [a] -> Bool
mynull [] = True
mynull (_:_) = False

myhead :: [a] -> a
myhead (x:xs) = x

mytail :: [a] -> [a]
mytail (x:xs) = xs 

--Requires NPlusKPatterns
--mypred :: Int -> Int
--mypred 0 = 0
--mypred (n + 1) = n

myconst :: a -> (b -> a)
myconst n = (\_ -> n)

odds :: Int -> [Int]
odds n = map (\x -> x * 2 + 1) [0..n-1]

plusn :: Int -> (Int -> Int)
plusn n = (+n)

halve :: [a] -> ([a], [a])
halve xs =  (\x -> (take x xs , drop x xs)) (length xs `div` 2)

safetailcond :: [a] -> [a]
safetailcond xs = if(null xs == True) then [] else tail xs

safetailguard :: [a] -> [a]
safetailguard xs | null xs == True = []
				| otherwise  = tail xs 

safetailpatternmatch :: [a] -> [a]
safetailpatternmatch [] = []
safetailpatternmatch (x:xs) = xs