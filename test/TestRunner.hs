--testes manuais, para roda-los corretamente faça 'ghci -isrc test/TestRunner.hs'
--assim 'runAll' dave atualizar os resultados em './Logs'

module TestRunner ( main ) where

import Parser.Parser ( programParser )
import AST.VisualAST ( showAST, saveAST )
import Lexer.Lexer ( lexFromFile )

main :: Int -> IO()
main num = do
    lexerResult <- lexFromFile ("./test/ex" ++ show num ++ ".py")
    case lexerResult of
        Left err -> putStrLn err
        Right tokens -> do
            print tokens
            putStrLn (showAST ast)
            saveAST ast ("./test/Logs/ex" ++ show num ++ "_ast.txt")
            where ast = programParser tokens

runAll :: [Int] -> IO()
runAll [] = return ()
runAll (h:t) = do
    main h
    runAll t
    