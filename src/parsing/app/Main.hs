module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
              let atom = first:rest
              return $ case atom of
                "#t" -> Bool True
                "#f" -> Bool False
                _    -> Atom atom

readParser :: String -> Parser Integer
readParser input = return $ read input

numberParser :: Integer -> Parser LispVal
numberParser input = return $ Number input

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- 1.1
-- parseNumber = do
--                 parsedDigits <- many1 digit
--                 let converted = read parsedDigits
--                 return $ Number converted
-- 1.2
-- todo: can i do this without intermediate functions?
parseNumber = many1 digit >>= readParser >>= numberParser

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
