--testes manuais, para roda-los corretamente faça 'ghci -isrc test/TestRunner.hs'
--assim 'runAll' dave atualizar os resultados em './Logs'

module TestRunner ( main, runAll ) where

import Control.Exception
import Parser.Parser ( programParser )
import AST.VisualAST ( saveAST )
import Lexer.Lexer ( lexFromFile )

main :: Int -> IO()
main num = do
    lexerResult <- lexFromFile ("./test/ex" ++ show num ++ ".py")
    case lexerResult of
        Left err -> putStrLn err
        Right tokens -> (do
            ast <- evaluate(programParser tokens) 
            saveAST ast ("./test/Logs/ex" ++ show num ++ "_ast.txt")) `catch` \e -> writeFile ("./test/Logs/ex" ++ show num ++ "_ast.txt") ("Erro ao analisar o arquivo: " ++ show (e :: ErrorCall))

runAll :: [Int] -> IO()
runAll [] = return ()
runAll (h:t) = do
    main h
    runAll t
    