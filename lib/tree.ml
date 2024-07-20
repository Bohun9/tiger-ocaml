(* The intermediate representation to which the abstract syntax is translated.
   It allows statements to be nested within expressions to make the main translation
   easier, but it needs to be simplified later by the `Canon` module.
 *)

type expr = 
  | EConst of int
  | ELabel of Temp.label
  | ETemp of Temp.temp
  | EBinop of expr * binop * expr
  | EMem of expr
  | ECall of expr * expr list
  | ESeq of stmt * expr
  [@@deriving show]

and stmt = 
  | SMove of expr * expr
  | SExpr of expr
  | SJump of expr * Temp.label list
  | SCJump of expr * relop * expr * Temp.label * Temp.label
  | SSeq of stmt * stmt
  | SLabel of Temp.label
  [@@deriving show]

and binop =
  | ADD | SUB | MUL | DIV
  [@@deriving show]

and relop =
  | EQ | NEQ
  | LT | LE | GT | GE
  [@@deriving show]

let not_rel relop = 
  match relop with
  | EQ -> NEQ
  | NEQ -> EQ
  | LT -> GE
  | GE -> LT
  | LE -> GT
  | GT -> LE
