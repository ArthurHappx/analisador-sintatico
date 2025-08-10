-- Modulo de funções implementadas para o funcionamento do CLI
module CLI.Args (parseArgs, Command(..)) where
import Data.List (isSuffixOf)

data Command
    = Analyze FilePath
    | AnalyzeSave FilePath
    | Help
    | Tests
    | Invalid
    deriving (Show)

parseArgs ::[String] -> IO Command
parseArgs args = do
    case args of
        ["--help"] -> return Help
        ["-tests"] -> return Tests
        [file]  -> if ".py" `isSuffixOf` file then return (Analyze file)
                    else return Invalid
        [file, "-s"] -> if ".py" `isSuffixOf` file then return (AnalyzeSave file)
                    else return Invalid
        _ -> return Invalid
