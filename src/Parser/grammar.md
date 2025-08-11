### Grammar:

Program = {Line}

Line = Stmt | Expr

Stmt = ident "=" Expr

Expr = Term Expr'

Expr' = Addop Term Expr' | Ɛ

Term = Factor Term'

Term' = Mulop Factor Term' | Ɛ

Factor = "(" Expr ")" | Unary | Value

Unary = "-" Factor | "+" Factor
	
Value = number | ident

Addop = "+" | "-"

Mulop = "*" | "/"


---
### Caption:

(a) - uma ou mais ocorrências de a
[a] - zero ou uma ocorrência de a
{a} - zero ou mais ocorrências de a
