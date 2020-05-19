%token <string> ID
%token BACKSLASH
%token DOT COLON
%token ARROW
%token BOOL TRUE FALSE IF THEN ELSE
%token LPAREN RPAREN
%token EQ
%token DOUBLESEMI
%token EOF

%start <Syntax.binding option> main

%right ARROW

%%

main:
| x = ID EQ e = expr DOUBLESEMI                    { Some (Syntax.Val (x, e)) }
| e = expr DOUBLESEMI                              { Some (Syntax.Eval e) }
| EOF                                              { None }

raw_atomic_expr:
| x = ID                                           { Syntax.Var x }
| TRUE                                             { Syntax.Bool true }
| FALSE                                            { Syntax.Bool false }
| LPAREN e = raw_expr RPAREN                       { e }
                                                   
atomic_expr: l = located(raw_atomic_expr)          { l }
                                                   
raw_app_expr:                                      
| e1 = app_expr e2 = atomic_expr                   { Syntax.App (e1, e2) }
| e = raw_atomic_expr                              { e }
                                                   
app_expr:                                          
| l = located(raw_app_expr)                        { l }
                                                   
raw_expr:                                          
| BACKSLASH x = ID COLON t = ty DOT e = expr       { Syntax.Lambda (x, t, e) }
| IF e1 = expr THEN e2 = expr ELSE e3 = expr       { Syntax.IfThenElse (e1, e2, e3) }
| e = raw_app_expr                                 { e }
                                                   
expr: l = located(raw_expr)                        { l }
                                                   
raw_atomic_ty:                                     
| BOOL                                             { Syntax.Ty.Bool }
| LPAREN t = raw_ty RPAREN                         { t }
                                                   
raw_ty:
| t1 = ty ARROW t2 = ty                            { Syntax.Ty.Fun (t1, t2) }
| t = raw_atomic_ty                                { t }
                                                   
ty: l = located(raw_ty)                            { l }
                                                   
located(V):                                        
| v = V                                            { { Syntax.loc = Some $loc; value = v } }
