module AST.VisualAST ( showAST, saveAST ) where

import Parser.Parser 

showAST :: [Line] -> String
showAST [] = ""
showAST (Stmt h:t) = showStmt h ++ showAST t
showAST (Expr h:t) = showExpr h ++ showAST t

showStmt :: Stmt -> String
showStmt (Assign name expr) =
    "\nAssign " ++ name ++ "\n" ++ indent (showExpr expr)

showExpr :: Expr -> String
showExpr (Plus e1 e2) = "└─Add\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (Minus e1 e2) = "└─Sub\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (Mult e1 e2) = "└─Mul\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (Div e1 e2) = "└─Div\n" ++ indent (showExpr e1) ++ indent (showExpr e2)
showExpr (UnaryOP op e) = "└─Unary " ++ op ++ "\n" ++ indent (showExpr e)
showExpr (Var s) = "└─Var " ++ s ++ "\n"
showExpr (Number n) = "└─Num " ++ n ++ "\n"

indent :: String -> String
indent = unlines . map ("  " ++) . lines

saveAST :: [Line] -> FilePath -> IO()
saveAST prog path = writeFile path (showAST prog)

defaultSaveAST :: [Line] -> IO()
defaultSaveAST prog = saveAST prog "../ast_results"

showAndSaveAST :: [Line] -> FilePath -> IO()
showAndSaveAST prog path = do 
    saveAST prog path
    putStrLn (showAST prog)
