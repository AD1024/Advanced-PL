%token <int> INT
%token <string> ID
%token TRUE FALSE
%token SKIP
%token PRINT READINT READBOOL ARRAYALLOC ARRAYLENGTH
%token NOT
%token PLUS
%token MINUS
%token DOUBLEAMP
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token COMMA
%token EQ
%token LE LT GE GT
%token ASSIGN
%token ASSERT
%token WHILE
%token LCURLY RCURLY
%token FOR FOREACH IN
%token IF ELSE
%token SEMISEP
%token EOF

%left SEMISEP
%left DOUBLEAMP
%left EQ LT GT LE GE
%left PLUS MINUS
%left LBRACKET

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
|  ELSE LCURLY x = X RCURLY { Some x }

%public optionParamCar(X):
| { None }
| x = X COMMA { Some x }

main:
| s = stmt EOF                               { s }


raw_subscript:
| id = expr LBRACKET i = expr RBRACKET        { Syntax.Subscript (id, i) }

subscript: l = located(raw_subscript)        { l }

raw_stmt:
| SKIP                                       { Syntax.Skip }
| id = ID ASSIGN e = expr                    { Syntax.Assign (id, e) }
| ASSERT e = expr                            { Syntax.Assert e }
| arr = subscript ASSIGN v = expr            { Syntax.AssignArr (arr, v) }
| WHILE cond = expr LCURLY s = stmt RCURLY   { Syntax.While (cond, s) }
| IF    cond = expr LCURLY lb = stmt
  RCURLY rb = optionElse(stmt)               { Syntax.If (cond, lb, rb) }
| PRINT LPAREN e = expr RPAREN               { Syntax.Print e }
| FOR 
    assign = stmt SEMISEP
    cond = expr SEMISEP
    upd = stmt
    LCURLY body = stmt RCURLY                { Syntax.For (assign, cond, upd, body) }
| FOREACH
    id = ID IN e = expr
    LCURLY body = stmt RCURLY                { Syntax.Foreach (id, e, body) }
| l = stmt SEMISEP r = stmt                  { Syntax.Seq (l, r) }

stmt: l = located(raw_stmt)                  { l }

raw_expr:
| i = INT                          { Syntax.Literal (Syntax.VInt i) }
| TRUE                             { Syntax.Literal (Syntax.VBool true) }
| FALSE                            { Syntax.Literal (Syntax.VBool false) }
| LPAREN RPAREN                    { Syntax.Literal (Syntax.VUnit) }
| x = ID                           { Syntax.Var x }
| e1 = expr EQ e2 = expr           { Syntax.Binop (e1, Syntax.Eq, e2)  }
| e1 = expr LT e2 = expr           { Syntax.Binop (e1, Syntax.Lt, e2)  }
| e1 = expr GT e2 = expr           { Syntax.Binop (e1, Syntax.Gt, e2)  }
| e1 = expr LE e2 = expr           { Syntax.Binop (e1, Syntax.Le, e2)  }
| e1 = expr GE e2 = expr           { Syntax.Binop (e1, Syntax.Ge, e2)  }
| e1 = expr PLUS e2 = expr         { Syntax.Binop (e1, Syntax.Add, e2) }
| e1 = expr MINUS e2 = expr        { Syntax.Binop (e1, Syntax.Sub, e2) }
| e1 = expr DOUBLEAMP e2 = expr    { Syntax.Binop (e1, Syntax.And, e2) }
| READBOOL LPAREN RPAREN           { Syntax.ReadBool }
| READINT  LPAREN RPAREN           { Syntax.ReadInt  }
| ARRAYLENGTH LPAREN 
  e = expr RPAREN                  { Syntax.ArrayLength e }
| ARRAYALLOC LPAREN
  init = expr
  COMMA length = expr RPAREN       { Syntax.ArrayAlloc (init, length) }
| MINUS e = expr  %prec UMINUS     { Syntax.Unop (Syntax.Neg, e) }
| NOT e = expr    %prec UNOT       { Syntax.Unop (Syntax.Not, e) }
| LBRACKET es = separated_list(COMMA, expr) RBRACKET { Syntax.Array es }
| s = raw_subscript                { s }
| LPAREN e = raw_expr RPAREN       { e }

expr: l = located(raw_expr)        { l }


located(V):
| v = V     { { Syntax.loc = $loc; value = v } }
