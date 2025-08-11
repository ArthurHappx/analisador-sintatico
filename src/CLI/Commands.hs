module CLI.Commands (executarComando) where

import Control.Exception
import Control.DeepSeq (deepseq)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath (takeBaseName)
import CLI.Args (Command(..))
import Lexer.Lexer (lexer)
import Parser.Parser (programParser)
import AST.VisualAST (showAST, saveAST)
-- import para funcionamento dos testes automáticos
import TestRunner (runAll)
import Data.List (isSuffixOf)

executarComando :: Command -> IO ()
executarComando (Analyze file) = analisarSimplificado file
executarComando (AnalyzeSave file) = analisarArquivo file
executarComando Help = mostrarAjuda
executarComando Tests = executarTestes
executarComando Invalid = putStrLn "Comando não identificado, tente '--help' para consultar os comandos válidos."

analisarArquivo :: FilePath -> IO ()
analisarArquivo file = do
    existe <- doesFileExist file
    if not existe
        then putStrLn "Arquivo não encontrado."
        else do
            putStrLn $ "Analisando arquivo: " ++ file
            result <- ( do
                code <- readFile file
                let tokens = lexer code
                let ast = programParser tokens
                ast `deepseq` return (Right ast)
                ) `catch` \e -> do
                    let eMsg = "Erro ao analisar o arquivo: " ++ show (e :: ErrorCall)
                    return (Left eMsg)

            case result of
                Right ast -> do
                    putStrLn "AST gerada:"
                    putStrLn (showAST ast)
                    saveAST ast ("./ast_results/" ++ takeBaseName file ++ "_ast.txt")
                    putStrLn ("AST salva em ./ast_results/" ++ takeBaseName file ++ "_ast.txt")
                Left eMsg -> do
                    writeFile ("./ast_results/" ++ takeBaseName file ++ "_ast.txt") eMsg
                    putStrLn eMsg

analisarSimplificado :: FilePath -> IO ()
analisarSimplificado file = do
    existe <- doesFileExist file
    if not existe
        then putStrLn "Arquivo não encontrado."
        else do
            putStrLn $ "Analisando arquivo: " ++ file
            result <- ( do
                code <- readFile file
                let tokens = lexer code
                let ast = programParser tokens
                ast `deepseq` return (Right ast)
                ) `catch` \e -> do
                    let eMsg = "Erro ao analisar o arquivo: " ++ show (e :: ErrorCall)
                    return (Left eMsg)

            case result of
                Right ast -> do
                    putStrLn "AST gerada:"
                    putStrLn (showAST ast)
                    putStrLn "AST não salva, para tanto, tente adicionar a clausula '-s'"
                Left eMsg -> do
                    putStrLn eMsg

mostrarAjuda :: IO ()
mostrarAjuda = do
    putStrLn "Uso:"
    putStrLn "./path/arquivo.py        # análise normal"
    putStrLn "./path/arquivo.py -s     # análise e salva o resultado em ./ast_results/arquivo.py"
    putStrLn "--help                   # mostra esta ajuda"
    putStrLn "-tests                   # executa testes internos e salva em ./test/Logs"
    putStrLn "exit()                   # encerra o programa"

executarTestes :: IO ()
executarTestes = do
    arquivos <- listDirectory "test/Examples"
    putStrLn "Executando testes internos..."
    let testes = [ f | f <- arquivos, ".py" `isSuffixOf` f]
    print testes
    TestRunner.runAll testes
