open Types

type enventry = 
  | VarEntry of { ty: ty }
  | FunEntry of { formals: ty list; result: ty }

let rec from_list xs = 
  match xs with
  | [] -> Symbol.empty
  | (k,v) :: xs -> Symbol.insert (from_list xs) k v

let base_tenv = from_list [
  "int", TInt;
  "string", TString;
]

let base_venv = from_list [
  "print", FunEntry { formals = [TString]; result = TUnit }
]
