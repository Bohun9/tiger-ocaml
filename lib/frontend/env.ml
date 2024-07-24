open Types

type enventry = 
  | VarEntry of {
      access: Translate.access;
      ty: ty
    }
  | FunEntry of {
      level: Translate.level option;
      label: Temp.label;
      formals: ty list;
      result: ty
    }

let rec from_list xs = 
  match xs with
  | [] -> Symbol.empty
  | (k, v) :: xs -> Symbol.insert (from_list xs) k v

let base_tenv = from_list [
  Symbol.symbol "int", TInt;
  Symbol.symbol "string", TString;
]

let builtin_functions = [
  "stringEqual", [TString; TString], TInt;
  "print", [TString], TUnit;
  "flush", [], TUnit;
  "ord", [TString], TInt;
  "chr", [TInt], TString;
  "size", [TString], TInt;
  "substring", [TString; TInt; TInt], TString;
  "concat", [TString; TString], TString;
  "not", [TInt], TString;
  "getChar", [], TString;
]

let base_venv : enventry Symbol.table =
  from_list (List.map (fun (name, formals, result) ->
    Symbol.symbol name, FunEntry { level = None; label = Symbol.symbol name; formals = formals; result = result }
  ) builtin_functions)
