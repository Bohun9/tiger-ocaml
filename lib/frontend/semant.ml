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

(* The types returned from functions [trans_exp], [trans_stmt], ..., and stored in the
   [tenv] and [venv] environments are normalised (i.e. applied to the actual_ty function).
 *)
let rec trans_exp (venv : venv) (tenv : tenv) (lvl : Translate.level) (ldone : Temp.label option) = 
  let rec tr_exp (e : exp) : expty = 
    match e with
    | EVar v -> trans_var venv tenv lvl ldone v
    | ENil -> { exp = Translate.nil_exp; ty = TNil }
    | EInt i -> { exp = Translate.int_exp i; ty = TInt }
    | EString s -> { exp = Translate.string_exp s; ty = TString }
    | ECall { func; args } ->
        let formals, result, fn_lvl, label = 
          match S.lookup venv func with
          | Some(Env.VarEntry _) -> failwith "the function name refers to a variable"
          | Some(Env.FunEntry { formals; result; level; label }) -> formals, result, level, label
          | None -> failwith "unbound function name"
        in
        let args = List.map (fun e -> tr_exp e) args in
        let rec check xs ys = 
          match (xs, ys) with
          | [], [] -> ()
          | (_::_), [] -> failwith "there are more formals than arguments"
          | [], (_::_) -> failwith "there are more formals than arguments"
          | (x::xs), (y::ys) -> unify_ty x y "the type of an argument does not match the type of a formal"; check xs ys
        in
        check formals (List.map (fun x -> x.ty) args);
        { exp = Translate.call_exp label fn_lvl lvl (List.map (fun x -> x.exp) args); ty = result }
    | EOp { e1; op; e2 } -> 
        let { exp = exp1; ty = t1 } = tr_exp e1 in
        let { exp = exp2; ty = t2 } = tr_exp e2 in
        let ty, string_eq =
          begin match op with
          | _ when List.mem op [OpAdd; OpSub; OpMul; OpDiv] ->
              unify_ty t1 TInt "the first operand of an arithmetic operation should be an integer";
              unify_ty t2 TInt "the second operand of an arithmetic operation should be an integer";
              TInt, false
          | _ when List.mem op [OpEq; OpNeq] ->
              unify_ty t1 t2 "operands of equality operators should be compatible";
              begin match t1 with
              | TString -> TInt, true
              | _ -> TInt, false
              end
          | _ ->
              begin match t1, t2 with
              | TInt, TInt -> TInt, false
              | _ -> failwith "wrong types of operands for a comparison operation"
              end
          end
        in
        { exp = Translate.op_exp exp1 op exp2 string_eq; ty = ty }
    | ERecord { typ; fields } -> 
        begin match S.lookup tenv typ with
        | Some (TRecord(fields_ty1, _) as t) ->
          let fields = List.map (fun (n, e) -> n, tr_exp e) fields in
          let fields_ty2 = List.map (fun (n, x) -> n, x.ty) fields in
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
          { exp = Translate.record_exp (List.map (fun (_, x) -> x.exp) fields); ty = t }
        | Some _ -> failwith "the type variable does not correspond to a record"
        | None -> failwith "unbound type variable"
        end
    | ESeq es ->
        begin match es with
        | [] -> { exp = Translate.nop; ty = TUnit }
        | [e] -> tr_exp e
        | e :: es ->
            let { exp = exp1; ty = t1 } = tr_exp e in
            unify_ty t1 TUnit "the non-last expression in an expression sequence should return unit";
            let { exp = exp2; ty = t2 } = tr_exp (ESeq es) in
            { exp = Translate.seq_exp exp1 exp2; ty = t2 }
        end
    | EAssign { var; e } ->
        let { exp = exp1; ty = t1 } = trans_var venv tenv lvl ldone var in
        let { exp = exp2; ty = t2 } = tr_exp e in
        unify_ty t1 t2 "type mismatch at the assignment operation";
        { exp = Translate.assign_exp exp1 exp2; ty = TUnit }
    | EIf { test; then'; else' } ->
        let { exp = exp1; ty = t1 } = tr_exp test in
        let { exp = exp2; ty = t2 } = tr_exp then' in
        let exp3, t3 = 
          begin match else' with
          | Some e -> let { exp = exp3; ty = t3 } = tr_exp e in exp3, t3
          | None -> Translate.nop, TUnit
          end
        in
        unify_ty t1 TInt "test expression in the if statement should be an integer";
        unify_ty t2 t3 "if branches should have compatible types"; 
        { exp = Translate.if_exp exp1 exp2 exp3; ty = t2 }
    | EWhile { test; body } ->
        let test_lab = Temp.new_label () in
        let { exp = exp1; ty = t1 } = tr_exp test in
        let { exp = exp2; ty = t2 } = trans_exp venv tenv lvl (Some test_lab) body in
        unify_ty t1 TInt "the test in a while statement should be an integer";
        unify_ty t2 TUnit "the body in a while statement should return unit";
        { exp = Translate.while_exp exp1 exp2 test_lab; ty = TUnit }
    | EFor { var; lo; hi; body; escape } ->
        tr_exp (
          ELet {
            decls = [
              DVar { var = var; escape = escape; annot = None; e = lo };
              DVar { var = Symbol.symbol "_for_hi"; escape = ref true; annot = None; e = hi };
            ]; 
            body = EWhile {
              test = EOp { e1 = EVar(VSimple var); op = OpLe; e2 = (EVar(VSimple (Symbol.symbol "_for_hi"))) };
              body = ESeq([
                body;
                EAssign { var = VSimple var; e = EOp { e1 = EVar(VSimple var); op = OpAdd; e2 = EInt 1 }};
              ])
            }
          }
        )
    | EBreak ->
        begin match ldone with
        | Some d -> { exp = Translate.break_exp d; ty = TUnit }
        | None -> failwith "the break is not inside a loop"
        end
    | ELet { decls; body } ->
        let (venv', tenv', es) = trans_decls venv tenv lvl ldone decls in
        let { exp = exp; ty = ty } = trans_exp venv' tenv' lvl ldone body in
        { exp = Translate.let_exp es exp; ty = ty }
    | EArray { typ; size; init } ->
        begin match S.lookup tenv typ with
        | Some (TArray(t1, _) as t) ->
            let { exp = exp1; ty = t2 } = tr_exp size in
            let { exp = exp2; ty = t3 } = tr_exp init in
            unify_ty t2 TInt "the size of an array should be an integer";
            unify_ty t1 t3 "the initinal value for an array has a wrong type";
            { exp = Translate.array_exp exp1 exp2; ty = t }
        | Some _ -> failwith "the type variable does not correspond to an array"
        | None -> failwith "unbound type variable"
        end
  in
  tr_exp

and trans_var (venv : venv) (tenv : tenv) (lvl : Translate.level) (ldone : Temp.label option) (var : var)  : expty = 
  match var with
  | VSimple v ->
      begin match S.lookup venv v with
      | Some (VarEntry { ty = t; access = access }) -> { exp = Translate.simple_var access lvl; ty = t }
      | Some (FunEntry _) -> failwith "the variable name refers to a function"
      | None -> failwith "unbound variable"
      end
  | VField(r, f) ->
      let { exp = exp; ty = t } = trans_var venv tenv lvl ldone r in
      begin match t with
      | TRecord(fields, _) -> 
          begin match List.assoc_opt f fields with
          | Some t ->
              let i = 
                begin match List.find_index (fun (f',_) -> f = f') fields with
                | Some i -> i
                | None -> failwith "trans_var(VField): internal error"
                end
              in
              { exp = Translate.field_var exp i; ty = actual_ty t }
          | None -> failwith "extracting a field which is not part of a record"
          end
      | _ -> failwith "extracting a field from a non-record"
      end
  | VSubscript(a, s) ->
      let { exp = exp1; ty = t1 } = trans_var venv tenv lvl ldone a in
      let { exp = exp2; ty = t2 } = trans_exp venv tenv lvl ldone s in
      match t1 with
      | TArray(t3, _) ->
          unify_ty t2 TInt "the subscript should be an integer";
          { exp = Translate.subscript_var exp1 exp2; ty = actual_ty t3 }
      | _ -> failwith "taking subscript of a non-array"

and trans_decls (venv : venv) (tenv : tenv) (lvl : Translate.level) (ldone : Temp.label option) (decls : decl list) : venv * tenv * Translate.exp list = 
  match decls with
  | [] -> venv, tenv, []
  | d :: ds ->
      let (venv', tenv', es1) = trans_dec venv tenv lvl ldone d in
      let (venv'', tenv'', es2) = trans_decls venv' tenv' lvl ldone ds in
      venv'', tenv'', es1 @ es2

and trans_dec (venv : venv) (tenv : tenv) (lvl : Translate.level) (ldone : Temp.label option) (decl : decl) : venv * tenv * Translate.exp list =
  match decl with
  | DVar { var; escape; annot; e } -> 
      let { exp = exp; ty = t1 } = trans_exp venv tenv lvl ldone e in
      begin match annot with
      | Some t2 ->
          begin match S.lookup tenv t2 with
          | Some t2 -> unify_ty t1 t2 "the annotation of a variable does not match the type of an initional expression"
          | None -> failwith "unbound type variable"
          end
      | None -> ()
      end;
      let access = Translate.alloc_local lvl !escape in
      let venv' = S.insert venv var (Env.VarEntry { ty = t1; access = access }) in
      venv', tenv, [Translate.var_dec access exp]
  | DFunctions fns ->
      let formals_ty params = 
        List.map (fun { typ = t; name = n; escape = e } ->
          match S.lookup tenv t with
          | Some t -> n, t, !e
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
          let label = Temp.new_label () in
          let new_level = Translate.new_level lvl label (List.map (fun p -> !(p.escape)) params) in
          let ent = Env.FunEntry {
            label = label;
            level = Some new_level;
            formals = List.map (fun (_, t, _) -> t) (formals_ty params);
            result = result_ty result;
          } in
          S.insert acc fname ent
        ) venv fns
      in
      let _ = List.iter
        (fun { fname; params; result; body; } ->
          match S.lookup venv' fname with
          | Some (Env.FunEntry { level = Some fn_level; _ }) ->
              let venv'' = List.fold_left
                (fun acc ((n, t, e), access) ->
                  S.insert acc n (Env.VarEntry { ty = t; access = access })
                ) venv' (List.combine (formals_ty params) (Translate.formals fn_level)) in
              let { exp = exp; ty = t } = trans_exp venv'' tenv fn_level None body in
              Translate.proc_entry_exit fn_level exp;
              unify_ty t (result_ty result) "the return type of a function does not match an annotation"
          | _ -> failwith "trans_dec(DFunctions): internal error"
        ) fns
      in
      venv', tenv, []
  | DTypes tys ->
      let tenv' = List.fold_left
        (fun acc (n, _) ->
          S.insert acc n (TName(n, ref None))
        ) tenv tys
      in
      let _ = List.iter
        (fun (n, t) ->
          match S.lookup tenv' n with
          | Some (TName(_, r)) -> r := Some (trans_ty tenv' t)
          | _ -> failwith "trans_dec - DTypes: internal error"
        ) tys
      in
      let _ = List.iter
        (fun (n, _) -> 
          let t = S.lookup_unsafe tenv' n in
          let rec check t vis = 
            match t with
            | TName(n, r) ->
                begin match !r with
                | None -> failwith "trans_dec(DTypes): internal error"
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
      let tenv'' = List.fold_left (
        fun acc (n, _) ->
          match S.lookup tenv' n with
          | Some(TName(_, r)) ->
              begin match !r with
              | Some t -> S.insert acc n t
              | None -> failwith "trans_dec(DTypes): internal error"
              end
          | _ -> failwith "trans_dec(DTypes): internal error"
      ) tenv tys
      in
      venv, tenv'', []

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

let trans_prog (p : exp) : unit = 
  let { exp = exp; ty = _ } = trans_exp Env.base_venv Env.base_tenv Translate.outermost None p in
  Translate.proc_entry_exit Translate.outermost exp
