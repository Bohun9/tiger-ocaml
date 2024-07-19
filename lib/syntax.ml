type var = 
  | VSimple of Symbol.symbol
  | VField of var * Symbol.symbol
  | VSubscript of var * exp
  [@@deriving show]

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
  [@@deriving show]

and decl =
  | DFunctions of fundecl list
  | DVar of { var: Symbol.symbol; escape: bool ref; annot: Symbol.symbol option; e: exp }
  | DTypes of (Symbol.symbol * typ) list
  [@@deriving show]

and typ =
  | TName of Symbol.symbol
  | TRecord of field list
  | TArray of Symbol.symbol
  [@@deriving show]

and field = { name: Symbol.symbol; escape: bool ref; typ: Symbol.symbol }
  [@@deriving show]

and fundecl = { fname: Symbol.symbol; params: field list; result: Symbol.symbol option; body: exp }
  [@@deriving show]

and op = 
  | OpAdd | OpSub | OpMul | OpDiv
  | OpEq | OpNeq
  | OpLt | OpLe | OpGt | OpGe

let rec string_of_var = function
  | VSimple s -> "VSimple(" ^ s ^ ")"
  | VField (v, s) -> "VField(" ^ string_of_var v ^ ", " ^ s ^ ")"
  | VSubscript (v, e) -> "VSubscript(" ^ string_of_var v ^ ", " ^ string_of_exp e ^ ")"

and string_of_exp = function
  | EVar v -> "EVar(" ^ string_of_var v ^ ")"
  | ENil -> "ENil"
  | EInt i -> "EInt(" ^ string_of_int i ^ ")"
  | EString s -> "EString(" ^ s ^ ")"
  | ECall {func; args} -> 
      "ECall { func = " ^ func ^ "; args = [" ^ String.concat "; " (List.map string_of_exp args) ^ "] }"
  | EOp {e1; op; e2} -> 
      "EOp { e1 = " ^ string_of_exp e1 ^ "; op = " ^ string_of_op op ^ "; e2 = " ^ string_of_exp e2 ^ " }"
  | ERecord {typ; fields} -> 
      "ERecord { typ = " ^ typ ^ "; fields = [" ^ String.concat "; " (List.map (fun (s, e) -> "(" ^ s ^ ", " ^ string_of_exp e ^ ")") fields) ^ "] }"
  | ESeq es -> "ESeq [" ^ String.concat "; " (List.map string_of_exp es) ^ "]"
  | EAssign {var; e} -> "EAssign { var = " ^ string_of_var var ^ "; e = " ^ string_of_exp e ^ " }"
  | EIf {test; then'; else'} -> 
      "EIf { test = " ^ string_of_exp test ^ "; then' = " ^ string_of_exp then' ^ 
      "; else' = " ^ (match else' with Some e -> string_of_exp e | None -> "None") ^ " }"
  | EWhile {test; body} -> "EWhile { test = " ^ string_of_exp test ^ "; body = " ^ string_of_exp body ^ " }"
  | EFor {var; escape; lo; hi; body} -> 
      "EFor { var = " ^ var ^ "; escape = " ^ string_of_bool !escape ^ "; lo = " ^ string_of_exp lo ^ 
      "; hi = " ^ string_of_exp hi ^ "; body = " ^ string_of_exp body ^ " }"
  | EBreak -> "EBreak"
  | ELet {decls; body} -> 
      "ELet { decls = [" ^ String.concat "; " (List.map string_of_decl decls) ^ "]; body = " ^ string_of_exp body ^ " }"
  | EArray {typ; size; init} -> 
      "EArray { typ = " ^ typ ^ "; size = " ^ string_of_exp size ^ "; init = " ^ string_of_exp init ^ " }"

and string_of_decl = function
  | DFunctions fundecls -> "DFunctions [" ^ String.concat "; " (List.map string_of_fundecl fundecls) ^ "]"
  | DVar {var; escape; annot; e} -> 
      "DVar { var = " ^ var ^ "; escape = " ^ string_of_bool !escape ^ 
      "; anot = " ^ (match annot with Some a -> a | None -> "None") ^ "; e = " ^ string_of_exp e ^ " }"
  | DTypes types -> 
      "DTypes [" ^ String.concat "; " (List.map (fun (s, t) -> "(" ^ s ^ ", " ^ string_of_ty t ^ ")") types) ^ "]"

and string_of_ty = function
  | TName s -> "TName(" ^ s ^ ")"
  | TRecord fields -> "TRecord [" ^ String.concat "; " (List.map string_of_field fields) ^ "]"
  | TArray s -> "TArray(" ^ s ^ ")"

and string_of_field {name; escape; typ} = 
  "{ name = " ^ name ^ "; escape = " ^ string_of_bool !escape ^ "; typ = " ^ typ ^ " }"

and string_of_fundecl {fname; params; result; body} = 
  "{ fname = " ^ fname ^ "; params = [" ^ String.concat "; " (List.map string_of_field params) ^ 
  "]; result = " ^ (match result with Some r -> r | None -> "None") ^ "; body = " ^ string_of_exp body ^ " }"

and string_of_op = function
  | OpAdd -> "OpAdd"
  | OpSub -> "OpSub"
  | OpMul -> "OpMul"
  | OpDiv -> "OpDiv"
  | OpEq -> "OpEq"
  | OpNeq -> "OpNeq"
  | OpLt -> "OpLt"
  | OpLe -> "OpLe"
  | OpGt -> "OpGt"
  | OpGe -> "OpGe"
