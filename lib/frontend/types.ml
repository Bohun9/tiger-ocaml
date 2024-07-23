type unique = unit ref

type ty = 
  | TInt
  | TString
  | TUnit
  | TNil
  | TRecord of (Symbol.symbol * ty) list * unique
  | TArray of ty * unique
  | TName of Symbol.symbol * ty option ref

(* The [TName] constructor is used to allow recursive types. It will be filled with [None]
   only during processing of type declarations.
*)

let rec string_of_ty ty = 
  match ty with
  | TInt -> "int"
  | TString -> "str"
  | TUnit -> "()"
  | TNil -> "nil"
  | TRecord(fields, _) -> Printf.sprintf "rcd {%s}" (String.concat "," (List.map (fun (n, t) -> Printf.sprintf "%s=%s" (Symbol.name n) (string_of_ty t)) fields))
  | TArray(t, _) -> Printf.sprintf "[%s]" (string_of_ty t)
  | TName(n, _) -> Printf.sprintf "TName %s" (Symbol.name n)

let eq_ty t1 t2 = 
  match t1, t2 with
  | TInt, TInt -> true
  | TString, TString -> true
  | TUnit, TUnit -> true
  | TNil, TNil -> true
  | TArray(_, u1), TArray(_, u2) -> u1 = u2
  | TRecord(_, u1), TRecord(_, u2) -> u1 = u2
  | TNil, TRecord(_, _) | TRecord(_, _), TNil -> true
  | TName(_, _), _ -> false | _, TName(_, _) -> false
  | _, _ -> false

let rec actual_ty t = 
  match t with
  | TName(_, r) -> 
      begin match !r with
      | Some t -> actual_ty t
      | None -> failwith "actual_ty: internal error"
      end
  | t -> t
