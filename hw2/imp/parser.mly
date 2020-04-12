%token <int> INT
%token <string> ID
%token TRUE FALSE
%token SKIP
%token NOT
%token PLUS
%token MINUS
%token DOUBLEAMP
%token LPAREN RPAREN
%token EQ
%token LE
%token ASSIGN
%token ASSERT
%token WHILE
%token LCURLY RCURLY
%token IF THEN ELSE
%token END
%token SEMISEP
%token EOF

%left SEMISEP
%left DOUBLEAMP
%left EQ LE
%left PLUS MINUS

(* Declare a special precedence for a fake token UMINUS, which 
   is not used by the lexer at all, and exists only to adjust
   the parsing precedence of unary negation, which is different 
   from the parsing precedence of the binary operation subtraction. *) 
%nonassoc UMINUS
%nonassoc UNOT

%start <Syntax.stmt> main

(* These type annotations aren't strictly necessary,
   but they improve error messages when you make a typo in the grammar. *)
%type <Syntax.raw_expr> raw_expr
%type <Syntax.expr> expr
%type <Syntax.raw_stmt> raw_stmt
%type <Syntax.stmt> stmt

%%


%public optionElse(X):
|  { None }
|  ELSE x = X { Some x }

main:
| s = stmt EOF                               { s }

raw_stmt:
| SKIP                                       { Syntax.Skip }
| id = ID ASSIGN e = expr                    { Syntax.Assign (id, e) }
| ASSERT e = expr                            { Syntax.Assert e }
| WHILE cond = expr LCURLY s = stmt RCURLY   { Syntax.While (cond, s) }
| l = stmt SEMISEP r = stmt                  { Syntax.Seq (l, r) }

stmt: l = located(raw_stmt)                  { l }

raw_expr:
| i = INT                          { Syntax.Literal (Syntax.VInt i) }
| TRUE                             { Syntax.Literal (Syntax.VBool true) }
| FALSE                            { Syntax.Literal (Syntax.VBool false) }
| LPAREN RPAREN                    { Syntax.Literal (Syntax.VUnit) }
| x = ID                           { Syntax.Var x }
| e1 = expr EQ e2 = expr           { Syntax.Binop (e1, Syntax.Eq, e2)  }
| e1 = expr LE e2 = expr           { Syntax.Binop (e1, Syntax.Le, e2)  }
| e1 = expr PLUS e2 = expr         { Syntax.Binop (e1, Syntax.Add, e2) }
| e1 = expr MINUS e2 = expr        { Syntax.Binop (e1, Syntax.Sub, e2) }
| e1 = expr DOUBLEAMP e2 = expr    { Syntax.Binop (e1, Syntax.And, e2) }
| IF cond = expr THEN 
   ifbranch = expr 
   elsebranch = optionElse(expr) END         {Syntax.Ite (cond, ifbranch, elsebranch)}
| MINUS e = expr  %prec UMINUS     { Syntax.Unop (Syntax.Neg, e) }
| NOT e = expr    %prec UNOT       { Syntax.Unop (Syntax.Not, e) }
| LPAREN e = raw_expr RPAREN       { e }

expr: l = located(raw_expr)        { l }


located(V):
| v = V     { { Syntax.loc = $loc; value = v } }
