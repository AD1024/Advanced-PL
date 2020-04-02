%token <int> INT
%token <bool> BOOL
%token AND
%token PLUS
%token LPAREN RPAREN
%token DOUBLESEMI
%token EOF

%left PLUS
%left AND

%start <Syntax.expr option> main

%%

main:
| e = expr DOUBLESEMI                 { Some e }
| EOF                                 { None }

expr:
| i = INT                          { Syntax.Literal (Val i) }
| b = BOOL                         { Syntax.Bool (Val b) }
| e1 = expr AND e2 = expr          { Syntax.And (e1, e2) }
| e1 = expr PLUS e2 = expr         { Syntax.Add (e1, e2) }
| LPAREN e = expr RPAREN           { e }
