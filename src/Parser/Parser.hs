module Parser (programParser, Expr(..), ASTs(..), Stmt(..), Token(..), Line(..)) where

newtype ASTs = Program [Line]
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

programParser :: [Token] -> ASTs
programParser [] = error "Not valid program"
programParser tokens = Program (parseLineMap 1 lines)
 where lines = lineSplit tokens

parseLineMap :: Int -> [[Token]] -> [Line]
parseLineMap lineNum [] = []
parseLineMap lineNum (line : rest') = parseLine  lineNum line : parseLineMap (lineNum+1) rest'

lineSplit :: [Token] -> [[Token]]
lineSplit [] = error "Not valid program"
lineSplit [EOF] = [[]]
lineSplit (token:rest') = 
    if token == NEWLINE then [] : lines
    else (token : head lines ) : safeTail lines
 where lines = lineSplit rest'

parseLine :: Int -> [Token] -> Line
parseLine lineNum [] = error "Not valid program"
parseLine lineNum (TkIdent var : TkAssign : rest') = LineStmt (parseStmt lineNum (TkIdent var : TkAssign : rest') )
parseLine lineNum line = LineExpr (parseExpr lineNum line)

parseStmt :: Int -> [Token] -> Stmt
parseStmt lineNum (TkIdent var : TkAssign : rest') = Assign var (parseExpr lineNum rest')
parseStmt lineNum _ = error (errorCaller lineNum 4)

parseExpr :: Int -> [Token] -> Expr
parseExpr lineNum [] = error (errorCaller lineNum 4)
parseExpr lineNum x
    | opPos == (-1) = parseTerm lineNum x
    | x !! opPos == TkPlus = Plus (parseExpr lineNum (take opPos x)) (parseTerm lineNum (drop (opPos+1) x))
    | x !! opPos == TkMinus = Minus (parseExpr lineNum (take opPos x)) (parseTerm lineNum (drop (opPos+1) x))
    | otherwise = error (errorCaller lineNum 4)
    where opPos =  findExpr lineNum (-1) 0 x 

findExpr :: Int -> Int -> Int -> [Token] -> Int
findExpr lineNum best pos [h] = best
findExpr lineNum best pos (h:t) 
    | h == TkOpenPar = findExpr lineNum best (pos + parPos) (drop parPos (h:t))
    | notElem h [TkPlus, TkMinus, TkMult, TkDiv] && head t `elem` [TkPlus, TkMinus] = findExpr lineNum (pos+1) (pos+1) t
    | otherwise = findExpr lineNum best (pos+1) t 
    where parPos = respPar lineNum 0 0 (h:t)

parseTerm :: Int -> [Token] -> Expr
parseTerm lineNum [] = error (errorCaller lineNum 4)
parseTerm lineNum x
    | opPos == (-1) = parseFactor lineNum x
    | x !! opPos == TkMult = Mult (parseTerm lineNum (take opPos x)) (parseFactor lineNum (drop (opPos+1) x))
    | x !! opPos == TkDiv = Div (parseTerm lineNum (take opPos x)) (parseFactor lineNum (drop (opPos+1) x))
    | otherwise = error (errorCaller lineNum 4)
    where opPos = findTerm lineNum (-1) 0 x 

findTerm :: Int -> Int -> Int -> [Token] -> Int
findTerm lineNum best pos [h] = best
findTerm lineNum best pos (h:t) 
    | h == TkOpenPar = findTerm lineNum best (pos + parPos) (drop parPos (h:t))
    | notElem h [TkPlus, TkMinus, TkMult, TkDiv] && head t `elem` [TkMult, TkDiv] = findTerm lineNum (pos+1) (pos+1) t
    | otherwise = findTerm lineNum best (pos+1) t 
    where parPos = respPar lineNum 0 0 (h:t)

respPar :: Int -> Int -> Int -> [Token] -> Int
respPar lineNum qnt pos [] = error (errorCaller lineNum 0)
respPar lineNum qnt pos (h:t)
    | (h == TkClosePar) && (qnt-1 == 0) = pos
    | h == TkClosePar = respPar lineNum (qnt-1) (pos+1) t
    | h == TkOpenPar = respPar lineNum (qnt+1) (pos+1) t
    | otherwise = respPar lineNum qnt (pos+1) t


parseFactor :: Int -> [Token] -> Expr
parseFactor lineNum [] = error (errorCaller lineNum 4)
parseFactor lineNum (TkOpenPar : rest') = parseExpr lineNum (safeInit rest')
parseFactor lineNum (TkMinus : rest') = parseUnary lineNum (TkMinus : rest')
parseFactor lineNum (TkPlus : rest') = parseUnary  lineNum (TkPlus : rest')
parseFactor lineNum (TkIdent name : rest') = parseValue lineNum (TkIdent name : rest')
parseFactor lineNum (TkNumber num : rest') = parseValue lineNum (TkNumber num : rest')
parseFactor lineNum (token:_) = if token `elem` [TkMult, TkDiv] then error (errorCaller lineNum 3) else error (errorCaller lineNum 4)

safeInit :: [t] -> [t] 
safeInit [] = []
safeInit xs = init xs

safeTail :: [t] -> [t] 
safeTail [] = []
safeTail xs = tail xs

parseUnary :: Int -> [Token] -> Expr
parseUnary lineNum [] = error (errorCaller lineNum 4)
parseUnary lineNum (TkMinus : rest') = UnaryOp "-" (parseFactor lineNum rest')
parseUnary lineNum (TkPlus : rest') = UnaryOp "+" (parseFactor lineNum rest')

parseValue :: Int -> [Token] -> Expr
parseValue lineNum [] = error (errorCaller lineNum 4)
parseValue lineNum [TkIdent name] = Var name
parseValue lineNum [TkNumber num] = Number num
parseValue lineNum (_ : rest') = if head rest' == TkClosePar then error (errorCaller lineNum 1) else error (errorCaller lineNum 2)

errorCaller :: Int -> Int -> String
errorCaller lineNum 0 = "SyntaxError: '(' was never closed, line " ++ show lineNum
errorCaller lineNum 1 = "SyntaxError: unmatched ')', line " ++ show lineNum
errorCaller lineNum 2 = "SyntaxError: invalid syntax, line " ++ show lineNum
errorCaller lineNum 3 = "SyntaxError: can't use starred expression here, line " ++ show lineNum
errorCaller lineNum 4 = "Unexpected error, line" ++ show lineNum