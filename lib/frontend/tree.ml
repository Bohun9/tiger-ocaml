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
  [@@deriving show { with_path = false }]

and stmt = 
  | SMove of expr * expr
  | SExpr of expr
  | SJump of expr * Temp.label list
  | SCJump of expr * relop * expr * Temp.label * Temp.label
  | SSeq of stmt * stmt
  | SLabel of Temp.label
  [@@deriving show { with_path = false }]

and binop =
  | ADD | SUB | MUL | DIV
  [@@deriving show { with_path = false }]

and relop =
  | EQ | NEQ
  | LT | LE | GT | GE
  [@@deriving show { with_path = false }]

let not_rel relop = 
  match relop with
  | EQ -> NEQ
  | NEQ -> EQ
  | LT -> GE
  | GE -> LT
  | LE -> GT
  | GT -> LE

let rec seq (ss : stmt list) : stmt = 
  match ss with
  | [] -> SExpr(EConst 0)
  | [s] -> s
  | s :: ss -> SSeq(s, seq ss)

