%{
  open Syntax

let rec merge_decls (decls: decl list) : decl list = 
  match decls with
  | [] -> []
  | DFunctions fs :: DFunctions fs' :: xs -> merge_decls (DFunctions (fs @ fs') :: xs)
  | DTypes ts :: DTypes ts' :: xs -> merge_decls (DTypes (ts @ ts') :: xs)
  | x :: xs -> x :: merge_decls xs
%}

%type <Syntax.exp> file
%start file

%token EOF
%token <string> ID
%token <int> INT
%token <string> STR

%token NIL IF THEN ELSE WHILE DO FOR TO BREAK LET IN END OF VAR ARRAY FUNCTION TYPE
%token L_PAREN R_PAREN L_BRACE R_BRACE L_BRACKET R_BRACKET
%token AND OR EQ NEQ ADD SUB MUL DIV LT LE GT GE
%token COMMA DOT COLON SEMICOLON ASSIGN

%nonassoc ASSIGN
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left ADD SUB
%left MUL DIV

%%

var
  : ID           { VSimple $1 }
  | var_compound { $1 }

var_compound
  : var DOT ID                  { VField($1, $3) }
  | ID L_BRACKET exp R_BRACKET  { VSubscript(VSimple $1, $3) }
  | var L_BRACKET exp R_BRACKET { VSubscript($1, $3) }

exp
  : var                                { EVar $1 }
  | NIL                                { ENil }
  | INT                                { EInt $1 }
  | STR                                { EString $1 }
  | ID L_PAREN seq(exp, COMMA) R_PAREN { ECall {func = $1; args = $3} }

  | exp AND exp { EIf {test = $1; then' = $3; else' = Some(EInt 0) } }
  | exp OR exp  { EIf {test = $1; then' = EInt 1; else' = Some($3) } }
  | exp EQ exp  { EOp {e1 = $1; op = OpEq; e2 = $3 } }
  | exp NEQ exp { EOp {e1 = $1; op = OpNeq; e2 = $3 } }
  | exp ADD exp { EOp {e1 = $1; op = OpAdd; e2 = $3 } }
  | exp SUB exp { EOp {e1 = $1; op = OpSub; e2 = $3 } }
  | exp MUL exp { EOp {e1 = $1; op = OpMul; e2 = $3 } }
  | exp DIV exp { EOp {e1 = $1; op = OpDiv; e2 = $3 } }
  | exp LT exp  { EOp {e1 = $1; op = OpLt; e2 = $3 } }
  | exp LE exp  { EOp {e1 = $1; op = OpLe; e2 = $3 } }
  | exp GT exp  { EOp {e1 = $1; op = OpGt; e2 = $3 } }
  | exp GE exp  { EOp {e1 = $1; op = OpGe; e2 = $3 } }

  | ID L_BRACE seq(field_exp, COMMA) R_BRACE { ERecord { typ = $1; fields = $3 } }
  | L_PAREN seq(exp, SEMICOLON) R_PAREN                { ESeq $2 }
  | var ASSIGN exp                                     { EAssign { var = $1; e = $3 } }
  | IF exp THEN exp ELSE exp                           { EIf { test = $2; then' = $4; else' = Some($6) } }
  | IF exp THEN exp                                    { EIf { test = $2; then' = $4; else' = None } }
  | WHILE exp DO exp                                   { EWhile { test = $2; body = $4 } }
  | FOR ID ASSIGN exp TO exp DO exp                    { EFor { var = $2; escape = ref true; lo = $4; hi = $6; body = $8 } }
  | BREAK                                              { EBreak }
  | LET chain(decl) IN exp END                         { ELet { decls = merge_decls $2; body = $4 } }                            
  | ID L_BRACKET exp R_BRACKET OF exp                  { EArray { typ = $1; size = $3; init = $6 } }

decl
  : FUNCTION ID L_PAREN seq(field_ty, COMMA) R_PAREN EQ exp          { DFunctions [{ fname = $2; params = $4; result = None; body = $7 }] }
  | FUNCTION ID L_PAREN seq(field_ty, COMMA) R_PAREN COLON ID EQ exp { DFunctions [{ fname = $2; params = $4; result = Some $7; body = $9 }] }
  | VAR ID ASSIGN exp                                                { DVar { var = $2; escape = ref true; annot = None; e = $4 } }
  | VAR ID COLON ID ASSIGN exp                                       { DVar { var = $2; escape = ref true; annot = Some $4; e = $6 } }
  | TYPE ID EQ ty                                                    { DTypes [$2, $4] }

ty
  : ID                                   { TName $1 }
  | L_BRACE seq(field_ty, COMMA) R_BRACE { TRecord $2 }
  | ARRAY OF ID                          { TArray $3 }

field_exp
  : ID EQ exp { $1, $3 }

field_ty
  : ID COLON ID { { name = $1; escape = ref true; typ = $3 } }

seq(item, SEP)
  : /* empty */ { [] }
  | item seq_tail(item, SEP) { $1 :: $2 }

seq_tail(item, SEP)
  : /* empty */ { [] }
  | SEP item seq_tail(item, SEP) { $2 :: $3 }

chain(item)
  : /* empty */ { [] }
  | item chain(item)   { $1 :: $2 }

file : exp EOF { $1 }

%%
