module TestRunner ( main, runAll ) where

import Control.Exception
import Parser.Parser ( programParser )
import AST.VisualAST ( saveAST )
import Lexer.Lexer ( lexFromFile )
import System.FilePath (takeBaseName)

main :: FilePath -> IO()
main path = do
    lexerResult <- lexFromFile ("./test/Examples/" ++ path)
    case lexerResult of
        Left err -> putStrLn err
        Right tokens -> (do
            writeFile ("./test/Logs/" ++ takeBaseName path ++ "_tokens.txt") (show tokens ++ "\n")
            ast <- evaluate(programParser tokens) 
            saveAST ast ("./test/Logs/" ++ takeBaseName path ++ "_ast.txt")) `catch` \e -> writeFile ("./test/Logs/" ++ takeBaseName path ++ "_ast.txt") ("Erro ao analisar o arquivo: " ++ show (e :: ErrorCall))
           

runAll :: [FilePath] -> IO()
runAll [] = return ()
runAll (h:t) = do
    main h
    runAll t
    