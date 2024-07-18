open Types

type enventry = 
  | VarEntry of
      { access: Translate.access
      ; ty: ty
      }
  | FunEntry of
      { level: Translate.level
      ; label: Temp.label
      ; formals: ty list
      ; result: ty
      }

let rec from_list xs = 
  match xs with
  | [] -> Symbol.empty
  | (k, v) :: xs -> Symbol.insert (from_list xs) k v

let base_tenv = from_list [
  "int", TInt;
  "string", TString;
]

let base_venv : enventry Symbol.table = from_list []
  (* "print", FunEntry { formals = [TString]; result = TUnit } *)
