module CLI.Commands (executarComando) where

import Control.Exception
import System.Directory (listDirectory, doesFileExist)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName)
import CLI.Args (Command(..))
import Lexer.Lexer (lexer)
import Parser.Parser (programParser)
import AST.VisualAST (showAST, saveAST)
-- import para funcionamento dos testes automáticos
import TestRunner (runAll)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isDigit)

executarComando :: Command -> IO ()
executarComando (Analyze file) = analisarSimplificado file
executarComando (AnalyzeSave file) = analisarArquivo file
executarComando Help = mostrarAjuda
executarComando Tests = executarTestes
executarComando Invalid = putStrLn "Uso inválido. Tente --help"

analisarArquivo :: FilePath -> IO ()
analisarArquivo file = do
    existe <- doesFileExist file
    if existe
        then do
            putStrLn $ "Analisando arquivo: " ++ file
            code <- readFile file
            ( do
                tokens <- evaluate (lexer code)
                ast <- evaluate (programParser tokens)
                putStrLn "AST gerada:"
                putStrLn (showAST ast)
                saveAST ast ("./ast_results/ast_" ++ takeBaseName file ++ ".txt")
                putStrLn "AST salva em resultado_ast.txt"
                ) `catch` \e -> do 
                    let eMsg = "Erro ao analisar o arquivo: " ++ show (e :: ErrorCall)
                    writeFile ("./ast_results/ast_" ++ takeBaseName file ++ ".txt") eMsg
                    putStrLn eMsg
        else putStrLn "Arquivo não encontrado."

analisarSimplificado :: FilePath -> IO ()
analisarSimplificado file = do
    existe <- doesFileExist file
    if existe
        then do
            putStrLn $ "Análise simplificada de: " ++ file
            code <- readFile file
            ( do
                tokens <- evaluate (lexer code)
                ast <- evaluate (programParser tokens)
                putStrLn (showAST ast)
                ) `catch` \e -> putStrLn $ "Erro ao analisar o arquivo: " ++ show (e :: ErrorCall)
        else putStrLn "Arquivo não encontrado."

mostrarAjuda :: IO ()
mostrarAjuda = do
    putStrLn "Uso:"
    putStrLn "./analisador arquivo.py        # análise normal"
    putStrLn "./analisador arquivo.py -s     # análise e salva o resultado em ./ast_results"
    putStrLn "./analisador --help            # mostra esta ajuda"
    putStrLn "./analisador -tests            # executa testes internos e salva em ./test/Logs"
    exitSuccess

executarTestes :: IO ()
executarTestes = do
    arquivos <- listDirectory "test"
    putStrLn "Executando testes internos..."
    let testes = [ read (takeWhile isDigit (drop 2 f)) :: Int
                 | f <- arquivos, "ex" `isPrefixOf` f, ".py" `isSuffixOf` f]
    TestRunner.runAll testes
