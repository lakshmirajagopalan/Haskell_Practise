import Data.Char
newtype Parser a =  P (String -> [(a,String)])

failure :: Parser a
failure = P (\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
		[] -> []
		(x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a, String)] 
parse (P p) inp = p inp

instance Monad Parser where
	return v = P (\inp -> [(v, inp)])

	p >>= f = P (\inp -> case parse p inp of
						[] -> []
						[(v, out)] -> parse (f v) out)


p +++ q = P(\inp -> case parse p inp of
					[] -> parse q inp
					[(v,out)] -> [(v, out)])


sat :: (Char -> Bool) -> Parser Char
sat p = do 
		x <- item
		if p x then return x else failure 

lower :: Parser Char
lower = sat isLower

digit :: Parser Char
digit = sat isDigit
	
char :: Char -> Parser Char
char c = sat (==c)

space :: Parser Char
space = sat (isSpace)

alphanum :: Parser Char
alphanum = sat (isAlphaNum)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
 				char x
 				string xs
 				return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
			v <- p
			vs <- many p
			return (v:vs)

ltrim :: Parser ()
ltrim = do
		many space
		return ()

nat :: Parser Integer
nat = do
	  v <- many digit
	  return (read v)	

int :: Parser Integer
int = do 
		symbol "-"
		n <- natural
		return (0-n)

	+++ natural						 

ident :: Parser String
ident = do
		v <- lower
		vs <- many alphanum
		return (v:vs)

token :: Parser a -> Parser a
token p = do
		many space
		v <- p
		many space
		return v

identifier :: Parser String
identifier = token ident

natural :: Parser Integer
natural = token nat

symbol :: String -> Parser String
symbol str = token (string str)

list :: Parser [Integer]
list = do
		symbol "["
		v <- natural
		vs <- many (do 
					symbol ","
					natural
			) 
			
		symbol "]"
		return (v:vs)	

comment :: Parser ()
comment = do
			string "--"
			many (sat ( /= '\n'))
			char '\n'
			return ()


expr :: Parser Integer
expr = do 
		t <- term
		do
			symbol "+"
			e <- expr
			return (t + e)
			+++ do
					symbol "-"
					e <- expr
					return (t - e)	
					+++ return t	

term :: Parser Integer
term = do 
		f <- factor
		do
			symbol "*"
			t <- term
			return (f * t)
			+++ do
					symbol "/"
					t <- term
					return (div f t)
					+++ return f			

factor :: Parser Integer
factor = do
			symbol "("
			e <- expr
			symbol ")"
			return e
			
		+++ natural

eval :: String -> Integer
eval xs = case parse expr xs of
		[(n, [])] -> n
		[(_, out)] -> error("unsedinpput " ++ out)
		[] -> error "invalid input"
			
