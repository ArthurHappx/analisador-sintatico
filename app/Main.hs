module Main where

import CLI.Args (parseArgs)
import CLI.Commands (executarComando)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStr ">> "
    hFlush stdout
    input <- getLine
    if input == "exit()" then putStrLn "Fechando programa..."
    else do
        let args = words input
        command <- parseArgs args
        executarComando command
        putStrLn "\n"
        main
