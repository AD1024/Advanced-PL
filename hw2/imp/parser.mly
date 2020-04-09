%token <int> INT
%token <string> ID
%token TRUE FALSE
%token SKIP
%token PLUS
%token MINUS
%token DOUBLEAMP
%token LPAREN RPAREN
%token EOF

%left DOUBLEAMP
%left PLUS MINUS

(* Declare a special precedence for a fake token UMINUS, which 
   is not used by the lexer at all, and exists only to adjust
   the parsing precedence of unary negation, which is different 
   from the parsing precedence of the binary operation subtraction. *) 
%nonassoc UMINUS

%start <Syntax.stmt> main

(* These type annotations aren't strictly necessary,
   but they improve error messages when you make a typo in the grammar. *)
%type <Syntax.raw_expr> raw_expr
%type <Syntax.expr> expr
%type <Syntax.raw_stmt> raw_stmt
%type <Syntax.stmt> stmt

%%

main:
| s = stmt EOF                     { s }

raw_stmt:
| SKIP                             { Syntax.Skip }

stmt: l = located(raw_stmt)        { l }

raw_expr:
| i = INT                          { Syntax.Literal (Syntax.VInt i) }
| TRUE                             { Syntax.Literal (Syntax.VBool true) }
| FALSE                            { Syntax.Literal (Syntax.VBool false) }
| x = ID                           { Syntax.Var x }
| e1 = expr PLUS e2 = expr         { Syntax.Binop (e1, Syntax.Add, e2) }
| e1 = expr MINUS e2 = expr        { Syntax.Binop (e1, Syntax.Sub, e2) }
| e1 = expr DOUBLEAMP e2 = expr    { Syntax.Binop (e1, Syntax.And, e2) }
| MINUS e = expr  %prec UMINUS     { Syntax.Unop (Syntax.Neg, e) }
| LPAREN e = raw_expr RPAREN       { e }

expr: l = located(raw_expr)        { l }


located(V):
| v = V     { { Syntax.loc = $loc; value = v } }
