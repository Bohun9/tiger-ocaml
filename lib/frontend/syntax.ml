(* The abstract syntax of the Tiger language. *)

type var = 
  | VSimple of Symbol.symbol
  | VField of var * Symbol.symbol
  | VSubscript of var * exp
  [@@deriving show { with_path = false }]

and exp = 
  | EVar of var
  | ENil
  | EInt of int
  | EString of string
  | ECall of { func: Symbol.symbol; args: exp list }
  | EOp of { e1: exp; op: op; e2: exp }
  | ERecord of { typ: Symbol.symbol; fields: (Symbol.symbol * exp) list }
  | ESeq of exp list
  | EAssign of { var: var; e: exp }
  | EIf of { test: exp; then': exp; else': exp option }
  | EWhile of { test: exp; body: exp }
  | EFor of { var: Symbol.symbol; escape: bool ref; lo: exp; hi: exp; body: exp }
  | EBreak
  | ELet of { decls: decl list; body: exp }
  | EArray of { typ: Symbol.symbol; size: exp; init: exp }
  [@@deriving show { with_path = false }]

and decl =
  | DFunctions of fundecl list
  | DVar of { var: Symbol.symbol; escape: bool ref; annot: Symbol.symbol option; e: exp }
  | DTypes of (Symbol.symbol * typ) list
  [@@deriving show { with_path = false }]

and typ =
  | TName of Symbol.symbol
  | TRecord of field list
  | TArray of Symbol.symbol
  [@@deriving show { with_path = false }]

and field = { name: Symbol.symbol; escape: bool ref; typ: Symbol.symbol }
  [@@deriving show { with_path = false }]

and fundecl = { fname: Symbol.symbol; params: field list; result: Symbol.symbol option; body: exp }
  [@@deriving show { with_path = false }]

and op = 
  | OpAdd | OpSub | OpMul | OpDiv
  | OpEq | OpNeq
  | OpLt | OpLe | OpGt | OpGe
  [@@deriving show { with_path = false }]

