module Main where

import CLI.Args (parseArgs)
import CLI.Commands (executarComando)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    putStrLn "\nAnalisador Léxico-Sintático (v0.1.0.0)"
    putStrLn "Digite '--help' para main informações."
    runInputT defaultSettings loop

loop :: InputT IO()
loop = do
    minInput <- getInputLine ">>> "
    case minInput of 
        Nothing -> return ()
        Just input -> 
            case input of
                "exit()" -> return ()
                "" -> do 
                    outputStr "" 
                    loop 
                validInput -> do 
                    let args = words validInput
                    command <- liftIO $ parseArgs args
                    liftIO $ executarComando command
                    loop
