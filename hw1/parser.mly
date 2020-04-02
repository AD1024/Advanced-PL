%token <int> INT
%token <bool> BOOL
%token <string> IDENT
%token ASSIGN
%token AND
%token PLUS
%token LPAREN RPAREN
%token DOUBLESEMI
%token EOF

%left AND
%left PLUS


%start <Syntax.binding option> main

%%

main:
| id = IDENT ASSIGN e = expr DOUBLESEMI         { Some (Binding (Some id, e)) }
| e = expr DOUBLESEMI                           { Some (Binding (None, e)) }
| EOF                                           { None }

expr:
| i = INT                          { Syntax.Literal (VInt i) }
| b = BOOL                         { Syntax.Bool (VBool b) }
| v = IDENT                        { Syntax.Var v }
| e1 = expr AND e2 = expr          { Syntax.And (e1, e2) }
| e1 = expr PLUS e2 = expr         { Syntax.Add (e1, e2) }
| LPAREN e = expr RPAREN           { e }
