module Main where

import CLI.Args (parseArgs)
import CLI.Commands (executarComando)

main :: IO ()
main = do
    cmd <- parseArgs
    executarComando cmd
