-- Modulo de funções implementadas para o funcionamento do CLI
module CLI.Args (parseArgs, Command(..)) where

import System.Environment (getArgs)

data Command
    = Analyze FilePath
    | AnalyzeSave FilePath
    | Help
    | Tests
    | Invalid
    deriving (Show)

parseArgs :: IO Command
parseArgs = do
    args <- getArgs
    case args of
        ["--help"] -> return Help
        ["-tests"] -> return Tests
        [file] -> return (Analyze file)
        [file, "-s"] -> return (AnalyzeSave file)
        _ -> return Invalid
