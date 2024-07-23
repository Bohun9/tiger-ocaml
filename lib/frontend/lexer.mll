{
open Parser

let kw_map = [
  "nil", NIL;
  "if", IF;
  "then", THEN;
  "else", ELSE;
  "while", WHILE;
  "do", DO;
  "for", FOR;
  "to", TO;
  "break", BREAK;
  "let", LET;
  "in", IN;
  "end", END;
  "of", OF;
  "var", VAR;
  "array", ARRAY;
  "function", FUNCTION;
  "type", TYPE;
] |> List.to_seq |> Hashtbl.of_seq

let symbols_map = [
  "(", L_PAREN;
  ")", R_PAREN;
  "{", L_BRACE;
  "}", R_BRACE;
  "[", L_BRACKET;
  "]", R_BRACKET;

  "&", AND;
  "|", OR;
  "=", EQ;
  "<>", NEQ;
  "+", ADD;
  "-", SUB;
  "*", MUL;
  "/", DIV;
  "<", LT;
  "<=", LE;
  ">", GT;
  ">=", GE;

  ",", COMMA;
  ".", DOT;
  ":", COLON;
  ";", SEMICOLON;
  ":=", ASSIGN;
] |> List.to_seq |> Hashtbl.of_seq

let make_id s = 
  try
    Hashtbl.find kw_map s
  with
    Not_found -> ID s

let make_symbol s = 
  Hashtbl.find symbols_map s
}

let digits = ['0'-'9']
let int = digits+
let identifier = ['a'-'z' '_'] ['a'-'z' '_' '0'-'9']*
let symbols = ['(' ')' '{' '}' '[' ']' '&' '|' '=' '+' '-' '*' '/' '<' '>' ',' '.' ':' ';'] | "<>" | "<=" | ">=" | ":="

rule token = parse
  | [' ' '\t' '\n'] { token lexbuf }
  | int as i { INT (int_of_string i) }
  | identifier as s { make_id s }
  | symbols as s { make_symbol s }
  | eof { EOF }
