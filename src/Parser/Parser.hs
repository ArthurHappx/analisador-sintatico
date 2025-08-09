module Parser.Parser (programParser, Expr(..), AST(..), Stmt(..), Line(..)) where

import Lexer.Lexer ( Token(..) )

newtype AST = Program [Line]
    deriving (Show, Eq)

data Line = LineStmt Stmt | LineExpr Expr
    deriving(Show, Eq)

data Stmt = Assign String Expr
    deriving (Show, Eq)

data Expr
    = Number String
    | Var String
    | Plus Expr Expr
    | Minus Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | UnaryOp String Expr
    deriving (Show, Eq)

{- programParser ->
Função principal do programa que desencadeia a recursão de todas as outras funções.
Retorna uma lista de `Line` com todas as árvores de análise (ASTs) do programa.
-}
programParser :: [Token] -> AST
programParser [] = error "Not valid program"
programParser tokens = Program (parseLineMap 1 programLines)
    where programLines = lineSplit tokens

{- parseLineMap ->
Uma função `map` especializada para o programa, que recebe as linhas de tokens já separadas
e executa cada uma, diferenciando entre expressões e declarações.
-}
parseLineMap :: Int -> [[Token]] -> [Line]
parseLineMap _ [] = []
parseLineMap lineNum ([] : rest') = parseLineMap (lineNum+1) rest'
parseLineMap lineNum (line : rest') = parseLine lineNum line : parseLineMap (lineNum+1) rest'

{- lineSplit ->
Função que divide a lista de tokens em sublistas, cada uma representando uma linha do programa.
Constrói uma lista de listas de tokens, aceitando e finalizando corretamente a recursão apenas se o token `EOF`
estiver presente no final da lista. O token `NEWLINE` atua como delimitador de linha, criando uma nova sublista.
-}
lineSplit :: [Token] -> [[Token]]
lineSplit [] = error "Not valid program"
lineSplit [EOF] = [[]]
lineSplit (token:rest') =
    if token == NEWLINE then [] : programLines
    else (token : head programLines ) : safeTail programLines
    where programLines = lineSplit rest'

{- parseLine ->
Função responsável por iniciar a construção da AST de uma linha.
Diferencia se a linha é uma expressão ou uma declaração, repassando para o parser adequado.
-}
parseLine :: Int -> [Token] -> Line
parseLine _ [] = error "Not valid program"
parseLine lineNum (TkIdent var : TkAssign : rest') = LineStmt (parseStmt lineNum (TkIdent var : TkAssign : rest') )
parseLine lineNum line = LineExpr (parseExpr lineNum line)

{- parseStmt ->
Função responsável por identificar uma declaração.
Reconhece o padrão dos dois primeiros tokens (identificador seguido de `=`) e repassa o restante para ser analisado como expressão.
-}
parseStmt :: Int -> [Token] -> Stmt
parseStmt lineNum (TkIdent var : TkAssign : rest') = Assign var (parseExpr lineNum rest')
parseStmt lineNum _ = error (errorCaller lineNum 4)

{- parseExpr ->
Função responsável por analisar expressões.
Depende de uma função auxiliar que encontra o último operador de adição/subtração (AddOp),
garantindo a precedência correta entre operações.
-}
parseExpr :: Int -> [Token] -> Expr
parseExpr lineNum [] = error (errorCaller lineNum 4)
parseExpr lineNum x
    | opPos == (-1) = parseTerm lineNum x
    | x !! opPos == TkPlus = Plus (parseExpr lineNum (take opPos x)) (parseTerm lineNum (drop (opPos+1) x))
    | x !! opPos == TkMinus = Minus (parseExpr lineNum (take opPos x)) (parseTerm lineNum (drop (opPos+1) x))
    | otherwise = error (errorCaller lineNum 4)
    where opPos =  findExpr lineNum (-1) 0 x

{- findExpr ->
Função auxiliar de `parseExpr`.
Responsável por encontrar o último operador de adição/subtração que se encaixe corretamente,
ignorando operadores unários e operadores dentro de parênteses.
-}
findExpr :: Int -> Int -> Int -> [Token] -> Int
findExpr _ best _ [_] = best
findExpr lineNum best pos (h:t)
    | h == TkOpenPar = findExpr lineNum best (pos + parPos) (drop parPos (h:t))
    | notElem h [TkPlus, TkMinus, TkMult, TkDiv] && head t `elem` [TkPlus, TkMinus] = findExpr lineNum (pos+1) (pos+1) t
    | otherwise = findExpr lineNum best (pos+1) t
    where parPos = respPar lineNum 0 0 (h:t)
findExpr lineNum _ _ _ = error (errorCaller lineNum 4)

{- parseTerm ->
Função responsável por analisar os termos de uma expressão.
Semelhante à `parseExpr`, mas foca nos operadores de multiplicação e divisão, mantendo a precedência correta.
-}
parseTerm :: Int -> [Token] -> Expr
parseTerm lineNum [] = error (errorCaller lineNum 4)
parseTerm lineNum x
    | opPos == (-1) = parseFactor lineNum x
    | x !! opPos == TkMult = Mult (parseTerm lineNum (take opPos x)) (parseFactor lineNum (drop (opPos+1) x))
    | x !! opPos == TkDiv = Div (parseTerm lineNum (take opPos x)) (parseFactor lineNum (drop (opPos+1) x))
    | otherwise = error (errorCaller lineNum 4)
    where opPos = findTerm lineNum (-1) 0 x

{- findTerm ->
Função auxiliar de `parseTerm`.
Responsável por encontrar o último operador de multiplicação/divisão que se encaixe corretamente,
ignorando operadores dentro de parênteses ou usados como unários.
-}
findTerm :: Int -> Int -> Int -> [Token] -> Int
findTerm _ best _ [_] = best
findTerm lineNum best pos (h:t)
    | h == TkOpenPar = findTerm lineNum best (pos + parPos) (drop parPos (h:t))
    | notElem h [TkPlus, TkMinus, TkMult, TkDiv] && head t `elem` [TkMult, TkDiv] = findTerm lineNum (pos+1) (pos+1) t
    | otherwise = findTerm lineNum best (pos+1) t
    where parPos = respPar lineNum 0 0 (h:t)
findTerm lineNum _ _ _ = error (errorCaller lineNum 4)

{- respPar ->
Função auxiliar usada nos `finds`.
Conta os parênteses de abertura e fechamento para detectar quando um conjunto de parênteses é encerrado corretamente,
também alertando quando algum parêntese não possui par correspondente.
-}
respPar :: Int -> Int -> Int -> [Token] -> Int
respPar lineNum _ _ [] = error (errorCaller lineNum 0)
respPar lineNum qnt pos (h:t)
    | (h == TkClosePar) && (qnt-1 == 0) = pos
    | h == TkClosePar = respPar lineNum (qnt-1) (pos+1) t
    | h == TkOpenPar = respPar lineNum (qnt+1) (pos+1) t
    | otherwise = respPar lineNum qnt (pos+1) t

{- parseFactor ->
Função responsável por analisar os fatores da expressão.
Por meio de casamento de padrões, identifica se o fator é um valor atômico ou se ainda requer tratamento adicional.
-}
parseFactor :: Int -> [Token] -> Expr
parseFactor lineNum [] = error (errorCaller lineNum 4)
parseFactor lineNum (TkOpenPar : rest') = parseExpr lineNum (safeInit rest')
parseFactor lineNum (TkMinus : rest') = parseUnary lineNum (TkMinus : rest')
parseFactor lineNum (TkPlus : rest') = parseUnary  lineNum (TkPlus : rest')
parseFactor lineNum (TkIdent name : rest') = parseValue lineNum (TkIdent name : rest')
parseFactor lineNum (TkNumber num : rest') = parseValue lineNum (TkNumber num : rest')
parseFactor lineNum (token : _) = if token `elem` [TkMult, TkDiv] then error (errorCaller lineNum 3) else error (errorCaller lineNum 2)

{- parseUnary ->
Função responsável por valores unários.
Identifica o sinal de determinado valor (positivo ou negativo), que pode ser um valor complexo ou atômico, e repassa para tratamento.
-}
parseUnary :: Int -> [Token] -> Expr
parseUnary lineNum (TkMinus : rest') = UnaryOp "-" (parseFactor lineNum rest')
parseUnary lineNum (TkPlus : rest') = UnaryOp "+" (parseFactor lineNum rest')
parseUnary lineNum _ = error (errorCaller lineNum 4)

{- parseValue ->
Função responsável por tratar valores atômicos.
Retorna a representação de variáveis (`Var`) ou números (`Number`), a depender do token analisado.
-}
parseValue :: Int -> [Token] -> Expr
parseValue lineNum [] = error (errorCaller lineNum 4)
parseValue _ [TkIdent name] = Var name
parseValue _ [TkNumber num] = Number num
parseValue lineNum (_ : rest') = if head rest' == TkClosePar then error (errorCaller lineNum 1) else error (errorCaller lineNum 2)

{- errorCaller ->
Função auxiliar para consolidar os possíveis erros do programa.
Agrupa os erros internos do parser, sem considerar erros de etapas superiores, que possuem seu próprio tratamento.
-}
errorCaller :: Int -> Int -> String
errorCaller lineNum 0 = "SyntaxError: '(' was never closed, line " ++ show lineNum
errorCaller lineNum 1 = "SyntaxError: unmatched ')', line " ++ show lineNum
errorCaller lineNum 2 = "SyntaxError: invalid syntax, line " ++ show lineNum
errorCaller lineNum 3 = "SyntaxError: can't use starred expression here, line " ++ show lineNum
errorCaller lineNum 4 = "Unexpected error, line " ++ show lineNum
errorCaller _ _ = "? error, ? line"

-- FUNÇÕES ÚTEIS
safeInit :: [t] -> [t]
safeInit [] = []
safeInit xs = init xs

safeTail :: [t] -> [t]
safeTail [] = []
safeTail xs = tail xs