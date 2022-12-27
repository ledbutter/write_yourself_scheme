module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    --original: putStrLn ("Hello, " ++ args !! 0)
    -- #1
    -- firstName <- return (args !! 0)
    -- lastName <- return (args !! 1)
    -- putStrLn ("Hello, " ++ firstName ++ " " ++ lastName)
    -- #2
    -- num1 <- return (read (args !! 0))
    -- num2 <- return (read (args !! 1))
    -- sum <- return (num1 + num2)
    -- sumDisplay <- return (show (sum))
    -- putStrLn ("Sum: " ++ sumDisplay)
    -- #3
    putStrLn("Please Enter a Name: ")
    name <- getLine
    putStrLn("You entered: " ++ name)