-- Modulo de funções implementadas para o funcionamento do CLI
module CLI.Args (parseArgs, Command(..)) where	

import System.Environment (getArgs)

data Command				
    = Analyze FilePath
    | AnalyzeSimple FilePath
    | Help
    | Tests [String]
    | Invalid
    deriving (Show)

parseArgs :: IO Command
parseArgs = do
    args <- getArgs
    return $ case args of
        [file] -> Analyze file
        [file, "-s"] -> AnalyzeSimple file
        ["--help"] -> Help
        ("-tests":rest) -> Tests rest
        _ -> Invalid
