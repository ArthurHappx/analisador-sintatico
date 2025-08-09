module CLI.Commands
    ( runCommand
    , mostrarAjuda
    , executarTestes
    ) where

import System.Directory (listDirectory, doesFileExist)
import System.Exit (exitSuccess)
import CLI.Args (Command(..))

runCommand :: Command -> IO ()
runCommand (Analyze file) = analisarArquivo file
runCommand (AnalyzeSimple file) = analisarArquivoSimplificado file
runCommand Help = mostrarAjuda
runCommand (Tests args) = executarTestes args
runCommand Invalid = putStrLn "Uso inválido. Tente --help"

-- Implementações
analisarArquivo :: FilePath -> IO ()
analisarArquivo file = do
    existe <- doesFileExist file
    if existe
        then putStrLn $ "Analisando arquivo: " ++ file
        else putStrLn "Arquivo não encontrado."

analisarArquivoSimplificado :: FilePath -> IO ()
analisarArquivoSimplificado file = do
    existe <- doesFileExist file
    if existe
        then putStrLn $ "Análise simplificada de: " ++ file
        else putStrLn "Arquivo não encontrado."

mostrarAjuda :: IO ()
mostrarAjuda = do
    putStrLn "Uso:"
    putStrLn "./analisador arquivo.py         # análise normal"
    putStrLn "./analisador arquivo.py -s     # análise simplificada"
    putStrLn "./analisador --help            # mostra esta ajuda"
    putStrLn "./analisador -tests [args...]  # executa testes internos"
    exitSuccess

executarTestes :: [String] -> IO ()
executarTestes _ = do
    arquivos <- listDirectory "tests"
    putStrLn "Executando testes internos..."
    rodarTestes arquivos

rodarTestes :: [FilePath] -> IO ()
rodarTestes [] = return ()
rodarTestes (t:ts) = do
    putStrLn ("Rodando teste: " ++ t)
    rodarTestes ts
