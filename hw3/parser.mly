%token <string> ID
%token LAM
%token DOT
%token LPAREN RPAREN
%token DOUBLESEMI
%token EOF

%start <Syntax.expr option> main

%%

main:
| e = expr DOUBLESEMI                   { Some e }
| EOF                                   { None }

raw_expr:
| e = raw_subexpr                       { e }
| LAM x = ID DOT e = expr            { Syntax.Lam (x, e) }

expr: l = located(raw_expr)             { l }

raw_subexpr:
| a = raw_atom                          { a }
| e1 = subexpr e2 = atom             { Syntax.App (e1, e2) }

subexpr: l = located(raw_subexpr)       { l }

raw_atom:
| x = ID                                { Syntax.Var x }
| LPAREN e = raw_expr RPAREN            { e }

atom: l = located(raw_atom)             { l }

located(V):
| v = V                                 { { Syntax.loc = Some $loc; value = v } }
