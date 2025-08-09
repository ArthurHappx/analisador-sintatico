module Lexer where

import System.IO (readFile)
import Control.Exception (try, IOException)


data Token
    = TkIdent String
    | TkAssign
    | TkNumber String
    | TkPlus
    | TkMinus
    | TkDiv
    | TkMult
    | TkOpenPar
    | TkClosePar
    | NEWLINE
    | EOF
    deriving (Show, Eq)


lexFromFile :: FilePath -> IO (Either String [Token])
lexFromFile filePath = do
    result <- try (readFile filePath) :: IO (Either IOException String)
    case result of
        Left err -> return $ Left $ "Erro ao ler arquivo: " ++ show err
        Right content -> return $ Right $ lexer content


lexer :: String -> [Token]
lexer input = lexAll input

lexAll :: String -> [Token]
lexAll [] = [EOF]
lexAll (c:cs)
    | isSpace c && c == '\n' = NEWLINE : lexAll cs
    | isSpace c              = lexAll cs
    | isAlpha c              = lexIdentifier (c:cs)
    | isDigit c              = lexNumber (c:cs)
    | c == '='               = TkAssign : lexAll cs
    | c == '+'               = TkPlus : lexAll cs
    | c == '-'               = TkMinus : lexAll cs
    | c == '/'               = TkDiv : lexAll cs
    | c == '*'               = TkMult : lexAll cs
    | c == '('               = TkOpenPar : lexAll cs
    | c == ')'               = TkClosePar : lexAll cs
    | otherwise              = error $ "Caractere inesperado '" ++ [c] ++ "'"

lexIdentifier :: String -> [Token]
lexIdentifier input =
    let (ident, rest) = span isAlphaNum input
    in TkIdent ident : lexAll rest

lexNumber :: String -> [Token]
lexNumber input =
    let (numStr, rest) = span isDigit input
    in TkNumber numStr : lexAll rest



isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\r\n"


isNumber :: String -> Bool
isNumber = all isDigit


