double x = x + x
my_sum n = sum[1..n]

rec_sum [] = 0
rec_sum (x:xs) = x + rec_sum xs

qsort [] = []
qsort (x:xs) =  qsort smaller ++ [x] ++ qsort larger
				where 
					smaller = [a | a <- xs, a <= x]
					larger = [a | a <- xs, a > x]

rev_qsort [] = []
rev_qsort (x:xs) = rev_qsort larger ++ [x] ++ rev_qsort smaller
					where
						smaller = [a | a <- xs, a <= x]
						larger = [a | a <- xs, a > x]

uniq_qsort [] = []
uniq_qsort (x:xs) =  uniq_qsort smaller ++ [x] ++ uniq_qsort larger
				where 
					smaller = [a | a <- xs, a < x]
					larger = [a | a <- xs, a > x]					