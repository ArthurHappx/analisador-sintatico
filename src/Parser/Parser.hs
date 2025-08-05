newtype AST = Program [Stmt]
    deriving (Show)

data Stmt = Assign String Expr
    deriving (Show)

data Expr
    = Number String
    | Var String
    | Plus Expr Expr
    | Minus Expr Expr
    | Mult Expr Expr
    | Div Expr Expr
    | UnaryOp String Expr
    deriving (Show)

{-
x = 8 + 12
y = x - 3

TkIdent x, TkAssign, TkNumber 8, TkPlus, TkNumber 12, NEWLINE,
TkIdent y, TkAssign, TkIdent x, TkMinus, TkNumber 3, EOF
-}

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
    deriving (Show)


parseStmt :: [Token] -> Stmt
parseStmt (TkIdent var : TkAssign : rest) = Assign var (parseExpr rest)
parseStmt _ = error "ERROR"

parseExpr :: [Token] -> Expr
parseExpr x
    | rest' == [] = term'
    | head rest' == TkPlus = Plus term' (parseExpr rest')
    | head rest' == TkMinus = Sub term' (parseExpr rest')
    | otherwise = error "ERROR"
    where [term', rest'] = parseTerm x --REFATORAÇÃO NECESSÁRIA

parseTerm :: [Token] -> Expr
parseTerm x = error "NOT IMPLEMENTED YET"

parseFactor :: [Token] -> Expr
parseFactor (TkOpenPar : rest') = parseExpr (safeInit rest')
parseFactor (TkMinus : rest') = parseUnary (TkMinus : rest')
parseFactor (TkPlus : rest') = parseUnary (TkPlus : rest')
parseFactor (TkIdent name : rest') = parseValue (TkIdent name : rest')
parseFactor (TkNumber num : rest') = parseValue (TkNumber num : rest')

safeInit :: [Token] -> [Token] 
safeInit [] = []
safeInit xs = init xs

parseUnary :: [Token] -> Expr
parseUnary [] = error "ERROR"
parseUnary (TkMinus : rest') = UnaryOp "-" (parseFactor rest')
parseUnary (TkPlus : rest') = UnaryOp "+" (parseFactor rest')

parseValue :: [Token] -> Expr
parseValue [] = error "ERROR"
parseValue (TkIdent name : rest') = Var name
parseValue (TkNumber num : rest') = Number num


{-
area :: Forma -> Float
area Circle r = 2 * pi * r
area Retangle a b = a * b

data Forma 
    = Circle Int
    | Retangle Int Int 
-}
