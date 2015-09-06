import Data.Char
data Parser a = Parser (String -> [(a, String)])

myreturn :: a -> Parser a
myreturn v = Parser(\inp -> [(v, inp)])

failure :: Parser a
failure = Parser (\inp -> [])

item :: Parser Char
item = Parser (\inp -> case inp of
			 			[] -> []
			 			(x:xs) -> [(x, xs)])

parse :: Parser a  -> String -> [(a, String)]
parse (Parser p) inp = p inp

instance Monad Parser where 
	return = myreturn
	p >>= f = Parser(\inp -> case (parse p inp) of
								[] -> []
								[(v, out)] -> parse (f v) out)

p :: Parser(Char, Char)
p = do 
	x <- item
	z <-	item
	y <- item
	return (x, y)


-- instance Monad List where
-- 	return a = [a]
-- 	(x:xs) >>= f = (f x) ++ (xs >>= f)
-- 	[] >>= f = []

-- fn = do
-- 	x <- [1,2,3]
-- 	y <- [2,3,4]
-- 	return (x,y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser(\inp -> case parse p inp of
 				[] -> parse q inp
 				[(v, out)] -> [(v, out)])


sat :: (Char -> Bool) -> Parser Char
sat p = do 
		x <- item
		if p x then return x else failure

char :: Char -> Parser Char
char c = sat ( == c)

digit :: Parser Char
digit = sat isDigit

alphanum :: Parser Char
alphanum = sat isAlphaNum

lower :: Parser Char
lower = sat isLower

space :: Parser Char
space = sat isSpace

string :: String -> Parser String
string [] = return []
string (x:xs) = do
	 			char x 
	 			string xs
	 			return (x:xs)

many :: Parser a -> Parser[a]
many p = many1 p +++ return []


many1 :: Parser a -> Parser[a]
many1 p = do 
			v <- p
			vs <- many p
			return (v:vs)

ident :: Parser String
ident = do 
		x <- lower
		xs <- many alphanum			
		return (x:xs)

nat :: Parser Int 
nat = do
		xs <- many1 digit
		return (read xs)

mspace :: Parser ()
mspace = do 
		many space
		return ()


token :: Parser a -> Parser a
token p = do
			mspace
			v <- p
			mspace
			return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs) 


naturalNumberList :: Parser [Int]
naturalNumberList = do 
					symbol "["
					v <- natural
					vs <- many (do
					 			symbol ","
					 			natural)
					symbol "]"
					return (v:vs)