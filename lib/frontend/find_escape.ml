open Syntax

type depth = int
type esc_env = (depth * bool ref) Symbol.table

let rec traverse_exp (env : esc_env) (d : depth) (e : exp)  : unit = 
  match e with
  | EVar v -> traverse_var env d v
  | ENil | EInt _ | EString _ | EBreak -> ()
  | ECall { args = es; _ } | ESeq es -> List.iter (traverse_exp env d) es 
  | EOp { e1; e2; _ } | EWhile { test = e1; body = e2 } | EArray { size = e1; init = e2; _ } ->
      traverse_exp env d e1; traverse_exp env d e2
  | ERecord { fields; _ } -> 
      let es = List.map snd fields in
      List.iter (traverse_exp env d) es
  | EAssign { var; e } -> traverse_var env d var; traverse_exp env d e
  | EIf { test; then'; else' } ->
      traverse_exp env d test;
      traverse_exp env d then';
      begin match else' with
      | Some e' -> traverse_exp env d e'
      | None -> ()
      end
  | EFor { var; escape; lo; hi; body } ->
      escape := false;
      let env' = Symbol.insert env var (d, escape) in
      traverse_exp env' d lo;
      traverse_exp env' d hi;
      traverse_exp env' d body
  | ELet { decls; body } ->
      let env' = traverse_decls env d decls in
      traverse_exp env' d body

and traverse_var (env : esc_env) (d : depth) (v : var) : unit = 
  match v with
  | VSimple x ->
      begin match Symbol.lookup env x with
      | Some (d', esc) -> if d > d' then esc := true else ()
      | None -> failwith "unbound variable"
      end
  | VField(v, _) -> traverse_var env d v
  | VSubscript(v, e) ->
      traverse_var env d v;
      traverse_exp env d e

and traverse_decls (env : esc_env) (d : depth) (decls : decl list) : esc_env =
  List.fold_left (fun acc decl ->
    traverse_decl acc d decl
  ) env decls

and traverse_decl (env : esc_env) (d : depth) (decl : decl) : esc_env =
  match decl with
  | DFunctions fns ->
      List.iter (fun { params; body; _ } ->
        let env' =
          List.fold_left (fun acc { name; escape; _ } ->
            escape := false;
            Symbol.insert acc name (d + 1, escape)
          ) env params
        in
        traverse_exp env' (d + 1) body
      ) fns;
      env
  | DVar { var; escape; e; _ } ->
      traverse_exp env d e;
      escape := false;
      Symbol.insert env var (d, escape)
  | DTypes _ -> env

let find_escape (e : exp) : unit = 
  traverse_exp Symbol.empty 0 e
