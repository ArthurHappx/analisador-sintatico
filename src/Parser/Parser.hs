newtype AST = Program [Stmt]
    deriving (Show, Eq)

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


parseStmt :: [Token] -> Stmt
parseStmt (TkIdent var : TkAssign : rest) = Assign var (parseExpr rest)
parseStmt _ = error "ERROR"

parseExpr :: [Token] -> Expr
parseExpr x
    | opPos == (-1) = parseTerm x
    | x !! opPos == TkPlus = Plus (parseExpr(take opPos x)) (parseTerm (drop (opPos+1) x))
    | x !! opPos == TkMinus = Minus (parseExpr(take opPos x)) (parseTerm (drop (opPos+1) x))
    | otherwise = error "ERROR"
    where opPos =  findExpr (-1) 0 x 

findExpr :: Int -> Int -> [Token] -> Int
findExpr best pos [h] = best
findExpr best pos (h:t) 
    | h == TkOpenPar = findExpr best (pos + parPos) (drop parPos (h:t))
    | notElem h [TkPlus, TkMinus, TkMult, TkDiv] && head t `elem` [TkPlus, TkMinus] = findExpr (pos+1) (pos+1) t
    | otherwise = findExpr best (pos+1) t 
    where parPos = respPar 0 0 (h:t)

parseTerm :: [Token] -> Expr
parseTerm x
    | opPos == (-1) = parseFactor x
    | x !! opPos == TkMult = Mult (parseTerm(take opPos x)) (parseFactor (drop (opPos+1) x))
    | x !! opPos == TkDiv = Div (parseTerm(take opPos x)) (parseFactor (drop (opPos+1) x))
    | otherwise = error "ERROR"
    where opPos = findTerm (-1) 0 x 

findTerm :: Int -> Int -> [Token] -> Int
findTerm best pos [h] = best
findTerm best pos (h:t) 
    | h == TkOpenPar = findTerm best (pos + parPos) (drop parPos (h:t))
    | notElem h [TkPlus, TkMinus, TkMult, TkDiv] && head t `elem` [TkMult, TkDiv] = findTerm (pos+1) (pos+1) t
    | otherwise = findTerm best (pos+1) t 
    where parPos = respPar 0 0 (h:t)

respPar :: Int -> Int -> [Token] -> Int
respPar qnt pos [] = error "ERRO"
respPar qnt pos (h:t)
    | (h == TkClosePar) && (qnt-1 == 0) = pos
    | h == TkClosePar = respPar (qnt-1) (pos+1) t
    | h == TkOpenPar = respPar (qnt+1) (pos+1) t
    | otherwise = respPar qnt (pos+1) t


parseFactor :: [Token] -> Expr
parseFactor (TkOpenPar : rest') = parseExpr (safeInit rest')
parseFactor (TkMinus : rest') = parseUnary (TkMinus : rest')
parseFactor (TkPlus : rest') = parseUnary (TkPlus : rest')
parseFactor (TkIdent name : rest') = parseValue (TkIdent name : rest')
parseFactor (TkNumber num : rest') = parseValue (TkNumber num : rest')

safeInit :: [Token] -> [Token] 
safeInit [] = []
safeInit xs = init xs

safeTail :: [Token] -> [Token] 
safeTail [] = []
safeTail xs = tail xs

parseUnary :: [Token] -> Expr
parseUnary [] = error "ERROR"
parseUnary (TkMinus : rest') = UnaryOp "-" (parseFactor rest')
parseUnary (TkPlus : rest') = UnaryOp "+" (parseFactor rest')

parseValue :: [Token] -> Expr
parseValue [] = error "ERROR"
parseValue (TkIdent name : rest') = Var name
parseValue (TkNumber num : rest') = Number num