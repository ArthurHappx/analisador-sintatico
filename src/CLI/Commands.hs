module CLI.Commands (executarComando) where

import System.Directory (listDirectory, doesFileExist)
import System.Exit (exitSuccess)
import CLI.Args (Command(..))
import Lexer.Lexer (lexer)
import Parser.Parser (programParser)
import AST.VisualAST (showAST, saveAST)
-- import para funcionamento dos testes automáticos
import qualified TestRunner (runAll)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Char (isDigit)

executarComando :: Command -> IO ()
executarComando (Analyze file) = analisarArquivo file
executarComando (AnalyzeSimple file) = analisarSimplificado file
executarComando Help = mostrarAjuda
executarComando (Tests args) = executarTestes args
executarComando Invalid = putStrLn "Uso inválido. Tente --help"

analisarArquivo :: FilePath -> IO ()
analisarArquivo file = do
    existe <- doesFileExist file
    if existe
        then do
            putStrLn $ "Analisando arquivo: " ++ file
            code <- readFile file
            let tokens = lexer code
            let ast = programParser tokens
            putStrLn "AST gerada:"
            putStrLn (showAST ast)
            saveAST ast "resultado_ast.txt"
            putStrLn "AST salva em resultado_ast.txt"
        else putStrLn "Arquivo não encontrado."

analisarSimplificado :: FilePath -> IO ()
analisarSimplificado file = do
    existe <- doesFileExist file
    if existe
        then do
            putStrLn $ "Análise simplificada de: " ++ file
            code <- readFile file
            let tokens = lexer code
            let ast = programParser tokens
            putStrLn (showAST ast)
        else putStrLn "Arquivo não encontrado."

mostrarAjuda :: IO ()
mostrarAjuda = do
    putStrLn "Uso:"
    putStrLn "./analisador arquivo.py         # análise normal"
    putStrLn "./analisador arquivo.py -s     # análise simplificada"
    putStrLn "./analisador --help            # mostra esta ajuda"
    putStrLn "./analisador -tests [args...]  # executa testes internos"
    exitSuccess

executarTestes :: IO ()
executarTestes _ = do
    arquivos <- listDirectory "tests"
    putStrLn "Executando testes internos..."
    let testes = [ read (takeWhile isDigit (drop 2 f)) :: Int
                 | f <- arquivos, "ex" `isPrefixOf` f, ".py" `isSuffixOf` f]
    TestRunner.runAll testes
