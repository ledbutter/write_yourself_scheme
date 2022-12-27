module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    --original: putStrLn ("Hello, " ++ args !! 0)
    firstName <- return (args !! 0)
    lastName <- return (args !! 1)
    putStrLn ("Hello, " ++ firstName ++ " " ++ lastName)