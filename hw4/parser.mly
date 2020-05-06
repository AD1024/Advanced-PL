%token <int> INT
%token <string> ID
%token TRUE FALSE
%token WHILE IF ELSE ASSERT SKIP ASSUME
%token INVARIANT
%token PLUS
%token MINUS
%token STAR
%token DOUBLEAMP
%token DOUBLEPIPE
%token IMPLIES
%token BANG
%token COLONEQ
%token EQ NEQ
%token LT LE GT GE
%token LPAREN RPAREN
%token LBRACE RBRACE
%token SEMI
%token EOF

%right SEMI
%right IMPLIES
%left DOUBLEPIPE
%left DOUBLEAMP
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left STAR
%nonassoc UMINUS BANG

%start <Syntax.stmt> main

(* these type annotations aren't strictly necessary,
   but they improve error messages when you make a typo in the grammar *)
%type <Syntax.raw_expr> raw_expr

%type <Syntax.expr> expr
%type <Syntax.raw_stmt> raw_stmt
%type <Syntax.stmt> stmt

%%

main:
| s = stmt EOF                     { s }

raw_stmt:
| SKIP                             { Syntax.Skip }
| ASSERT e = expr                  { Syntax.Assert e }
| ASSUME e = expr                  { Syntax.Assume e }
| x = ID COLONEQ e = expr          { Syntax.Assign (x, e) }
| s1 = stmt SEMI s2 = stmt         { Syntax.Seq (s1, s2) }
| IF e = expr
  LBRACE s1 = stmt RBRACE
  ELSE LBRACE s2 = stmt RBRACE     { Syntax.If (e, s1, s2) }
| WHILE e = expr  invs = invariants 
  LBRACE s = stmt RBRACE           { Syntax.While (e, List.rev invs, s) }

invariants:
| (* empty *)                           { [] }
| invs = invariants INVARIANT e = expr  { e :: invs }

stmt: l = located(raw_stmt)        { l }

%inline
binop: 
| PLUS         { Syntax.Add }
| MINUS        { Syntax.Sub }
| STAR         { Syntax.Mul }
| DOUBLEAMP    { Syntax.And }
| DOUBLEPIPE   { Syntax.Or }
| IMPLIES      { Syntax.Implies }
| EQ           { Syntax.Eq }
| NEQ          { Syntax.Neq }
| LT           { Syntax.Lt }
| LE           { Syntax.Le }
| GT           { Syntax.Gt }
| GE           { Syntax.Ge }

raw_expr:
| i = INT                          { Syntax.Literal (Syntax.VInt i) }
| TRUE                             { Syntax.Literal (Syntax.VBool true) }
| FALSE                            { Syntax.Literal (Syntax.VBool false) }
| x = ID                           { Syntax.Var x }
| e1 = expr b = binop e2 = expr    { Syntax.Binop (e1, b, e2) }
| MINUS e = expr %prec UMINUS      { Syntax.Unop (Syntax.Neg, e) }
| BANG e = expr                    { Syntax.Unop (Syntax.Not, e) }
| LPAREN e = raw_expr RPAREN       { e }

expr: l = located(raw_expr)        { l }


located(V):
| v = V     { { Syntax.loc = Some $loc; value = v } }
