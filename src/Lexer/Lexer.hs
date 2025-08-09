module Lexer.Lexer ( Token(..), lexFromFile, lexer ) where

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
lexer input = lexAll 1 input

lexAll :: Int -> String -> [Token]
lexAll _ [] = [EOF]
lexAll lineNum (c:cs)
    | isSpace c && c == '\n' = NEWLINE : lexAll (lineNum+1) cs
    | isSpace c = lexAll lineNum cs
    | c == '.' = lexNumber lineNum (c:cs)  -- Caso especial para .5 (verifica primeiro)
    | isDigit c = lexNumber lineNum (c:cs)
    | isAlpha c = lexIdentifier lineNum (c:cs)
    | c == '='               = TkAssign : lexAll lineNum cs
    | c == '+'               = TkPlus : lexAll lineNum cs
    | c == '-'               = TkMinus : lexAll lineNum cs
    | c == '/'               = TkDiv : lexAll lineNum cs
    | c == '*'               = TkMult : lexAll lineNum cs
    | c == '('               = TkOpenPar : lexAll lineNum cs
    | c == ')'               = TkClosePar : lexAll lineNum cs
    | otherwise              = error $ "SyntaxError: invalid syntax '" ++ [c] ++ "', line " ++ show lineNum

lexIdentifier :: Int -> String -> [Token]
lexIdentifier lineNum input =
    let (ident, rest) = span isAlphaNum input
    in TkIdent ident : lexAll lineNum rest

lexNumber :: Int -> String -> [Token]
lexNumber lineNum input = 
    let (numStr, rest) = span isNumberChar input
        isFloat = '.' `elem` numStr
        (isValid, errorMsg) = validateNumber numStr isFloat
    in if isValid
        then TkNumber numStr : lexAll lineNum rest
        else error (errorMsg ++ " , line " ++ show lineNum)

validateNumber :: String -> Bool -> (Bool, String)
validateNumber numStr isFloat
    | null numStr = (False, "Empty number")
    | hasMultiplePoints = (False, "SyntaxError: invalid decimal literal with multiple points in '" ++ numStr ++ "'")
    | startsWithPoint = (False, "SyntaxError: float cannot start with '.' in '" ++ numStr ++ "'")
    | endsWithPoint = (False, "SyntaxError: float cannot end with '.' in '" ++ numStr ++ "'")
    | not isFloat && hasLeadingZeros = (False, "SyntaxError: leading zeros in decimal integer literals are not permitted in '" ++ numStr ++ "'")
    | otherwise = (True, "")
    where
        hasMultiplePoints = length (filter (== '.') numStr) > 1
        startsWithPoint = head numStr == '.'
        endsWithPoint = last numStr == '.'
        hasLeadingZeros = case numStr of
            '0':x:_ -> isDigit x
            _ -> False

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
