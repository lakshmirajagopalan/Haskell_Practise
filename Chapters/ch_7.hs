twice :: (a -> a) -> a -> a 	
twice f x = f (f x)		

quadruples :: Num a => a -> a
quadruples x = (twice (*2)) x 