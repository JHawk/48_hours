module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces, (<|>))
import Monad
import Control.Applicative hiding (many)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal  = Atom	    String
	      | List	    [LispVal]
	      | DottedList  [LispVal] LispVal
	      | Number	    Integer
	      | String	    String
	      | Bool	    Bool
	      | Char	    Char
		deriving (Show) 

parseString :: Parser LispVal
parseString = do  char '"'
		  x <- many $ escape
		  char '"'
		  return $ String x
	    where escape = replace <|> noneOf "\""
		  replace = char '\\' >> choice (zipWith replacer codes replaceWith) 
		  replacer c r = char c >> return r
		  codes = ['b',  'n',  'f',  'r',  't',  '\\', '\"', '/']
		  replaceWith = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseAtom :: Parser LispVal
parseAtom = do	first <- letter <|> symbol
		rest  <- many (letter <|> digit <|> symbol)
		let atom = [first] ++ rest
		return $ case atom of 
			    "#t" -> Bool True
			    "#f" -> Bool False
			    otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseNumber1 :: Parser LispVal
parseNumber1 = (Number . read) <$> many1 digit

parseNumber2 :: Parser LispVal
parseNumber2 = many1 digit >>= (return . Number . read)

parseChar :: Parser LispVal
parseChar = do	char '\''
		x <- letter
		char '\'' 
		return $ Char x

parseExpr :: Parser LispVal
parseExpr = parseChar
	  <|> parseAtom
	  <|> parseString
	  <|> parseNumber

readExpr :: String -> String
readExpr input =  case parse parseExpr "lisp" input of
		  -- case parse (spaces >> symbol) "lisp" input of
		    Left err  -> "No match: " ++ show err
		    Right val -> "Found value" ++ show val

_print s = putStrLn s
_convert s = show(1 + (read s)::Integer)

main :: IO ()
main = do args <- getArgs
	  sequence_ (map (_print . readExpr) args) 
