module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

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
                x <- many (noneOf "\"")
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

myRead :: String -> Parser Integer
myRead input = return $ read input

what :: Integer -> Parser LispVal
what input = return $ Number input

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- 1.1
-- parseNumber = do
--                 parsedDigits <- many1 digit
--                 let converted = read parsedDigits
--                 return $ Number converted
-- 1.2
--            Parser String
parseNumber = many1 digit >>= myRead >>= what

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
