open Syntax
open Types
module S = Symbol

type venv = Env.enventry Symbol.table
type tenv = ty Symbol.table

type expty = { exp : Translate.exp; ty : ty }

let unify_ty t1 t2 msg = 
  let t1 = actual_ty t1 in
  let t2 = actual_ty t2 in
  if eq_ty t1 t2 then
    ()
  else
    failwith msg

(*
  Types returned from functions trans_* and stored in the tenv and venv environments
  are normalised (i.e. applied to the actual_ty function).
*)

let rec trans_exp (venv : venv) (tenv : tenv) = 
  let rec tr_exp (e : exp) : expty = 
    match e with
    | EVar v -> trans_var venv tenv v
    | ENil -> { exp = (); ty = TNil }
    | EInt _ -> { exp = (); ty = TInt }
    | EString _ -> { exp = (); ty = TString }
    | ECall { func; args } ->
        let formals, result = 
          match S.lookup venv func with
          | Some(Env.VarEntry _) -> failwith "the function name refers to a variable"
          | Some(Env.FunEntry { formals; result }) -> formals, result
          | None -> failwith "unbound function name"
        in
        let args = List.map (fun e -> tr_exp e) args in
        let rec check xs ys = 
          match (xs, ys) with
          | [], [] -> ()
          | (_::_), [] -> failwith "there are more formals than arguments"
          | [], (_::_) -> failwith "there are more formals than arguments"
          | (x::xs),(y::ys) -> unify_ty x y "the type of an argument does not match the type of a formal"; check xs ys
        in
        check formals (List.map (fun { exp = _; ty = ty } -> ty) args);
        { exp = (); ty = result }
    | EOp { e1; op; e2 } -> 
        let { exp = _; ty = t1 } = tr_exp e1 in
        let { exp = _; ty = t2 } = tr_exp e2 in
        let ty =
          begin match op with
          | _ when List.mem op [OpAdd; OpSub; OpMul; OpDiv] ->
              unify_ty t1 TInt "the first operand of an arithmetic operation should be an integer";
              unify_ty t2 TInt "the second operand of an arithmetic operation should be an integer";
              TInt
          | _ when List.mem op [OpEq; OpNeq] ->
              unify_ty t1 t2 "operands of equality operators should be compatible"; TInt
          | _ ->
              begin match t1, t2 with
              | TInt, TInt | TString, TString -> TInt
              | _ -> failwith "wrong types of operands for a comparision operation"
              end
          end
        in
        { exp = (); ty = ty }
    | ERecord { typ; fields } -> 
        begin match S.lookup tenv typ with
        | Some (TRecord(fields_ty1, _) as t) ->
          let fields = List.map (fun (n, e) -> n, tr_exp e) fields in
          let fields_ty2 = List.map (fun (n, { exp = _; ty = t }) -> n, t) fields in
          let rec check xs ys = 
            begin match xs, ys with
            | [], [] -> ()
            | _ :: _, [] -> failwith "too little fields provided during record creation"
            | [], _ :: _ -> failwith "too many fields provided during record creation"
            | (n1, t1) :: xs, (n2, t2) :: ys -> 
                if n1 <> n2 then
                  failwith "record field name mismatch"
                else
                  unify_ty t1 t2 "record field type mismatch";
                  check xs ys
            end
          in
          check fields_ty1 fields_ty2; 
          { exp = (); ty = t }
        | Some _ -> failwith "the type variable does not correspond to a record"
        | None -> failwith "unbound type variable"
        end
    | ESeq es ->
        begin match es with
        | [] -> { exp = (); ty = TUnit }
        | [e] -> tr_exp e
        | e :: es ->
            let { exp = _; ty = t } = tr_exp e in
            unify_ty t TUnit "the non-last expression in an expression sequence should return unit";
            tr_exp (ESeq es)
        end
    | EAssign { var; e } ->
        let { exp = _; ty = t1 } = trans_var venv tenv var in
        let { exp = _; ty = t2 } = tr_exp e in
        unify_ty t1 t2 "type mismatch at the assignment operation";
        { exp = (); ty = TUnit }
    | EIf { test; then'; else' } ->
        let { exp = _; ty = t1 } = tr_exp test in
        let { exp = _; ty = t2 } = tr_exp then' in
        let t3 = 
          begin match else' with
          | Some e -> let { exp = _; ty = t3 } = tr_exp e in t3
          | None -> TUnit
          end
        in
        unify_ty t1 TInt "test expression in the if statement should be an integer";
        unify_ty t2 t3 "if branches should have compatible types"; 
        { exp = (); ty = TUnit }
    | EWhile { test; body } ->
        let { exp = _; ty = t1 } = tr_exp test in
        let { exp = _; ty = t2 } = tr_exp body in
        unify_ty t1 TInt "the test in a while statement should be an integer";
        unify_ty t2 TUnit "the body in a while statement should return unit";
        { exp = (); ty = TUnit }
    | EFor { var; lo; hi; body; _ } ->
        let { exp = _; ty = t1 } = tr_exp lo in
        let { exp = _; ty = t2 } = tr_exp hi in
        unify_ty t1 TInt "the lowerbound of a for loop should be an integer";
        unify_ty t2 TInt "the upperbound of a for loop should be an integer";
        let venv' = S.insert venv var (Env.VarEntry { ty = TInt }) in
        let { exp = _; ty = t3 } = trans_exp venv' tenv body in
        unify_ty t3 TUnit "the body of a for loop should return unit";
        { exp = (); ty = TUnit }
    | EBreak -> { exp = (); ty = TUnit }
    | ELet { decls; body } ->
        let (venv', tenv') = trans_decls venv tenv decls in
        trans_exp venv' tenv' body
    | EArray { typ; size; init } ->
        begin match S.lookup tenv typ with
        | Some (TArray(t1, _) as t) ->
            let { exp = _; ty = t2 } = tr_exp size in
            let { exp = _; ty = t3 } = tr_exp init in
            unify_ty t2 TInt "the size of an array should be an integer";
            unify_ty t1 t3 "the initinal value for an array has a wrong type";
            { exp = (); ty = t }
        | Some _ -> failwith "the type variable does not correspond to an array"
        | None -> failwith "unbound type variable"
        end
  in
  tr_exp

and trans_var (venv : venv) (tenv : tenv) (var : var) : expty = 
  match var with
  | VSimple v ->
      begin match S.lookup venv v with
      | Some (VarEntry { ty = t }) -> { exp = (); ty = t }
      | Some (FunEntry _) -> failwith "the variable name refers to a function"
      | None -> failwith "unbound variable"
      end
  | VField(r, f) ->
      let { exp = (); ty = t } = trans_var venv tenv r in
      begin match t with
      | TRecord(fields, _) -> 
          begin match List.assoc_opt f fields with
          | Some t -> { exp = (); ty = actual_ty t }
          | None -> failwith "extracting a field which is not part of a record"
          end
      | _ -> failwith "extracting a field from a non-record"
      end
  | VSubscript(a, s) ->
      let { exp = (); ty = t1 } = trans_var venv tenv a in
      let { exp = (); ty = t2 } = trans_exp venv tenv s in
      match t1 with
      | TArray(t3, _) ->
          unify_ty t2 TInt "the subscript should be an integer";
          { exp = (); ty = actual_ty t3 }
      | _ -> failwith "taking subscript of a non-array"

and trans_decls (venv : venv) (tenv : tenv) (decls : decl list) : venv * tenv = 
  match decls with
  | [] -> venv, tenv
  | d :: ds ->
      let (venv', tenv') = trans_dec venv tenv d in trans_decls venv' tenv' ds

and trans_dec (venv : venv) (tenv : tenv) (decl : decl) : venv * tenv =
  match decl with
  | DVar { var; escape = _; annot; e } -> 
      let { exp = (); ty = t1 } = trans_exp venv tenv e in
      begin match annot with
      | Some t2 ->
          begin match S.lookup tenv t2 with
          | Some t2 -> unify_ty t1 t2 "the annotation of a variable does not match the type of an initional expression"
          | None -> failwith "unbound type variable"
          end
      | None -> ()
      end;
      let venv' = S.insert venv var (Env.VarEntry { ty = t1 }) in
      venv', tenv
  | DFunctions fns ->
      let formals_ty params = 
        List.map (fun { typ = t; name = n; _ } ->
          match S.lookup tenv t with
          | Some t -> n, t
          | None -> failwith "unbound type variable in function parameter annotation"
        ) params
      in
      let result_ty result = 
        match result with
        | Some t ->
            begin match S.lookup tenv t with
            | Some t -> t
            | None -> failwith "unbound type variable in function result annotation"
            end
        | None -> TUnit
      in
      let venv' = List.fold_left
        (fun acc { fname; params; result; _ } -> 
          let ent = Env.FunEntry { formals = List.map snd (formals_ty params); result = result_ty result } in
          S.insert acc fname ent
        ) venv fns
      in
      let _ = List.iter
        (fun { params; result; body; _ } ->
          let venv'' = List.fold_left
            (fun acc (n, t) ->
              S.insert acc n (Env.VarEntry { ty = t })
            ) venv' (formals_ty params) in
          let { exp = (); ty = t } = trans_exp venv'' tenv body in
          unify_ty t (result_ty result) "the return type of a function does not match an annotation"
        ) fns
      in
      venv', tenv
  | DTypes tys ->
      (* add headers to tenv  *)
      let tenv' = List.fold_left
        (fun acc (n, _) ->
          S.insert acc n (TName(n, ref None))
        ) tenv tys
      in
      (* create conceptually infinite types *)
      let _ = List.iter
        (fun (n, t) ->
          match S.lookup tenv' n with
          | Some (TName(_, r)) -> r := Some (trans_ty tenv' t)
          | _ -> failwith "trans_dec - DTypes: internal error"
        ) tys
      in
      (* check for cycles *)
      let _ = List.iter
        (fun (n, _) -> 
          let t = S.lookup_unsafe tenv' n in
          let rec check t vis = 
            match t with
            | TName(n, r) ->
                begin match !r with
                | None -> failwith "internal error"
                | Some t -> 
                    if not (List.mem n (List.map fst tys)) then
                      ()
                    else if List.mem n vis then
                      failwith "cyclic types"
                    else
                      check t (n :: vis)
                end
            | _ -> ()
          in
          check t []
        ) tys
      in
      (* normalise added types *)
      let tenv'' = List.fold_left (
        fun acc (n, _) ->
          match S.lookup tenv' n with
          | Some(TName(_, r)) ->
              begin match !r with
              | Some t -> S.insert acc n t
              | None -> failwith "internal error"
              end
          | _ -> failwith "internal error"
      ) tenv tys
      in
      venv, tenv''

and trans_ty (tenv : tenv) (ty : typ) : ty = 
  match ty with
  | TName n ->
      begin match S.lookup tenv n with
      | Some t -> t
      | None -> failwith "unbound type variable"
      end
  | TRecord fields ->
      let fields' = List.map (fun { name; typ; _ } -> name, trans_ty tenv (TName typ)) fields in
      TRecord(fields', ref ())
  | TArray n ->
      let t = trans_ty tenv (TName n) in
      TArray(t, ref ())

let trans_prog (p : exp) : ty = 
  (trans_exp Env.base_venv Env.base_tenv p).ty
