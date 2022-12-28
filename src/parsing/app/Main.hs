module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

escaped :: Parser Char
escaped = do
            char '\\'
            x <- oneOf "\\\"nrt" 
            return $ case x of 
                '\\' -> x
                '"'  -> x
                'n'  -> '\n'
                'r'  -> '\r'
                't'  -> '\t'

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
                char '"'
                -- original: x <- many (noneOf "\"")
                x <- many $ escaped <|> noneOf "\"\\"
                char '"'
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              return $ Atom (first:rest)

readParser :: String -> Parser Integer
readParser input = return $ read input

numberParser :: Integer -> Parser LispVal
numberParser input = return $ Number input

parseBool :: Parser LispVal
parseBool = do
                char '#'
                (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseDecimalBare :: Parser LispVal
parseDecimalBare = many1 digit >>= (return . Number . read)

parseDecimalDecorated :: Parser LispVal
parseDecimalDecorated = do try $ string "#d"
                           x <- many1 digit
                           (return . Number . read) x

parseBinary1 :: Parser LispVal
parseBinary1 = do try $ string "#b"
                  x <- many1 (oneOf "10")
                  return $ Number (bin2dig x)

bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseOctal1 :: Parser LispVal
parseOctal1 = do try $ string "#o"
                 x <- many1 octDigit
                 return $ Number (oct2dig x)

oct2dig x = fst $ readOct x !! 0

hex2dig x = fst $ readHex x !! 0

parseHex1 :: Parser LispVal
parseHex1 = do try $ string "#x"
               x <- many1 hexDigit
               return $ Number (hex2dig x)


parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- 1.1
-- parseNumber = do
--                 parsedDigits <- many1 digit
--                 let converted = read parsedDigits
--                 return $ Number converted
-- 1.2
-- todo: can i do this without intermediate functions?
--parseNumber = many1 digit >>= readParser >>= numberParser
-- with help: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Answers#Part_2
-- parseNumber = many1 digit >>= return . Number . read
-- 4
parseNumber = parseDecimalBare
    <|> parseDecimalDecorated
    <|> parseBinary1
    <|> parseOctal1
    <|> parseHex1

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseBool
