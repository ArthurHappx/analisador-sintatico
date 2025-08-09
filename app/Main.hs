module Main where

import CLI.Args (parseArgs, Command(..))
import CLI.Commands (mostrarAjuda, executarTestes)
import Parser.Parser (programParser)
import AST.VisualAST (showAST, saveAST)
import Lexer.Lexer (lexer) -- provisório enquanto o Lexer não fica pronto 

import System.Directory (doesFileExist)

main :: IO ()
main = do
    cmd <- parseArgs
    case cmd of
        Analyze file -> analisarArquivo file
        AnalyzeSimple file -> analisarArquivoSimples file
        Help -> mostrarAjuda
        Tests args -> executarTestes args
        Invalid -> putStrLn "Comando inválido. Use --help para ver as opções."

-- Análise completa + salva AST em arquivo
analisarArquivo :: FilePath -> IO ()
analisarArquivo file = do
    existe <- doesFileExist file
    if existe
        then do
            code <- readFile file
            let tokens = lexer code -- provisório enquanto o lexer não fica pronto
            let ast = programParser tokens
            putStrLn "AST gerada:"
            putStrLn (showAST ast)
            saveAST ast "resultado_ast.txt"
            putStrLn "AST salva em resultado_ast.txt"
        else putStrLn "Arquivo não encontrado."

-- Análise simplificada (só mostra AST no terminal)
analisarArquivoSimples :: FilePath -> IO ()
analisarArquivoSimples file = do
    existe <- doesFileExist file
    if existe
        then do
            code <- readFile file
            let tokens = lexer code
            let ast = programParser tokens
            putStrLn (showAST ast)
        else putStrLn "Arquivo não encontrado."
