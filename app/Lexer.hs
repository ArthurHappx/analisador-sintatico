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
        Left err -> return $ Left $ "ReadFileError " ++ show err
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
    | otherwise              = error $ "SintaxError: invalid sintax '" ++ [c] ++ "'"

lexIdentifier :: String -> [Token]
lexIdentifier input =
    let (ident, rest) = span isAlphaNum input
    in TkIdent ident : lexAll rest

-- Implementação atualizada de lexNumber
lexNumber :: String -> [Token]
lexNumber input = 
    let (numStr, rest) = span isNumberChar input
        -- Verifica se é um float válido
        (isValid, errorMsg) = validateNumber numStr
    in if isValid
        then TkNumber numStr : lexAll rest
        else error errorMsg

-- Validação completa do número
validateNumber :: String -> (Bool, String)
validateNumber numStr
    | hasMultiplePoints = (False, "SyntaxError: invalid syntax'")
    | hasLeadingZeros   = (False, "SyntaxError: leading zeros in decimal integer literals are not permitted'" ++ numStr ++ "'")
    | otherwise         = (True, "")
    where
        hasMultiplePoints = length (filter (== '.') numStr) > 1
        hasLeadingZeros = case numStr of
            '0':'.':_ -> False  -- 0.x é válido
            '0':x:_    -> x /= '.' && isDigit x 
            _          -> False

-- Função auxiliar para caracteres válidos em números
isNumberChar :: Char -> Bool
isNumberChar c = isDigit c || c == '.'


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


