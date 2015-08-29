myinsert :: Ord a => a -> [a] -> [a]
myinsert x [] = [x]
myinsert y (x:xs) | y <= x = y : x : xs
					|otherwise = x : (myinsert y xs)


isort :: Ord a => [a] -> [a]
isort [] = []
isort (x : xs)	= myinsert x (isort xs)

mydrop :: Int -> [a] -> [a]
mydrop 0 xs = xs
mydrop x [] = []
mydrop n (_:xs) = mydrop (n-1) xs

myzip :: [a] -> [b] -> [(a,b)]
myzip [] _ = []
myzip _ [] = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys


myeven :: Int -> Bool
myeven 0 = True
myeven n = myodd (n-1)

myodd :: Int -> Bool
myodd 0 = False
myodd n = myeven (n-1)

mymerge :: Ord a => [a] -> [a] -> [a]
mymerge [] ys = ys
mymerge xs [] = xs
mymerge (x:xs)(y:ys) | x <=y = x : (mymerge xs (y:ys))
					|otherwise = y : (mymerge (x:xs) ys) 

msort :: Ord a => [a]-> [a]
msort [] = []
msort [x] = [x]
msort xs = mymerge (msort (take ((length xs)`div` 2) xs)) (msort (drop ((length xs) `div` 2) xs)) 
			
twice :: (a -> a) -> a -> a 	
twice f x = f (f x)				