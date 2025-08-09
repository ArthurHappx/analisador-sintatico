module AST.VisualAST ( showAST, saveAST ) where

import Parser.Parser (Expr(..), Stmt(..), Line(..), AST(..))

showAST :: AST -> String
showAST (Program lns) = readLine lns
    where 
    readLine [] = ""
    readLine (LineStmt h:t) = showStmt h ++ readLine t
    readLine (LineExpr h:t) = "\nAloneExpr\n" ++ showExpr h ++ readLine t

showStmt :: Stmt -> String
showStmt (Assign name expr) =
    "\nAssign " ++ name ++ "\n" ++ indent (showExpr expr)

showExpr :: Expr -> String
showExpr (Plus e1 e2) = "└─Add\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (Minus e1 e2) = "└─Sub\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (Mult e1 e2) = "└─Mul\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (Div e1 e2) = "└─Div\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (UnaryOp op e) = "└─Unary " ++ op ++ "\n" ++ indent (showExpr e)
showExpr (Var s) = "└─Var " ++ s ++ "\n"
showExpr (Number n) = "└─Num " ++ n ++ "\n"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

saveAST :: AST -> FilePath -> IO()
saveAST ast path = writeFile path (showAST ast)
