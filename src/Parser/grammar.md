### Grammar:

Program = {Line}

Line = Stmt | Expr

Stmt = ident "=" Expr

Expr = Term [Addop Expr]

Term = Factor [Mulop Term]

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
