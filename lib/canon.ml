(* The main task of this module is to linearize the representation produced by the
   `Translate` module to make it closer to the assembly language.

   We achieve that by eliminating all `ESeq` constructors. Furthermore, we should
   also break expressions so that they don't affect the control flow. In this case,
   we achieve this by saving each function call to a new temporary.

   Additionally, the `CJump` instruction in the `Tree` module contains two jump 
   targets. We must transform the program so that each `CJump` is followed by
   its false label.
 *)

[@@@ocaml.warning "-8"] (* Trust me. *)

module T = Tree

let nop = T.SExpr(T.EConst 0)

let linearize (s0 : T.stmt) : T.stmt list = 
  let (%) s1 s2 = 
    match s1, s2 with
    | T.SExpr(T.EConst _), s -> s
    | s, T.SExpr(T.EConst _) -> s
    | s1, s2 -> T.SSeq(s1, s2)
  in
  let commute s e =
    match s, e with
    | T.SExpr(T.EConst _), _ -> true
    | _, T.EConst _ -> true
    | _, T.ELabel _ -> true
    | _ -> false
  in
  (* The functions `do_stmt` and `do_expr` linearize statements and expressions, respectively.
     Most of the cases are very similar: we have a bunch of subexpressions that must be solved recursively
     and then we need to merge the resulting statements together and put clean expressions back into the constructor.
     We can abstract this problem into a new function, `reorder`. 
   *)
  let rec reorder es =
    match es with
    | [] -> (nop, [])
    (* When a call is nested within a statement or expression, it should be moved into a new temporary. *)
    | T.ECall(_, _) as e :: es ->
        let t = Temp.new_temp () in
        reorder (T.ESeq(T.SMove(T.ETemp t, e), T.ETemp t) :: es)
    | e :: es ->
        let (s1, e) = do_expr e in
        let (s2, es) = reorder es in
        if commute s2 e then
          (s1 % s2, e :: es)
        else
          let t = Temp.new_temp () in
          s1 % (T.SMove(T.ETemp t, e)) % s2, T.ETemp t :: es
  and reorder_expr es build = 
    let (s, es) = reorder es in
    s, build es
  and reorder_stmt es build =
    let (s, es) = reorder es in
    s % build es
  and do_stmt s =
    match s with
    | T.SSeq(s1, s2) -> do_stmt s1 % do_stmt s2
    | T.SJump(e, labs) -> 
        reorder_stmt [e] (fun [e] -> T.SJump(e, labs))
    | T.SCJump(e1, op, e2, l1, l2) ->
        reorder_stmt [e1; e2] (fun [e1; e2] -> T.SCJump(e1, op, e2, l1, l2))
    | T.SMove(T.ETemp t, T.ECall(e, es)) ->
        reorder_stmt (e :: es) (fun (e :: es) -> T.SMove(T.ETemp t, T.ECall(e, es)))
    | T.SMove(T.ETemp t, e) ->
        reorder_stmt [e] (fun [e] -> T.SMove(T.ETemp t, e))
    | T.SMove(T.EMem e1, e2) ->
        reorder_stmt [e1; e2] (fun [e1; e2] -> T.SMove(T.EMem e1, e2))
    | T.SMove(T.ESeq(s, e1), e2) ->
        do_stmt (T.SSeq(s, T.SMove(e1, e2)))
    | T.SExpr(T.ECall(e, es)) ->
        reorder_stmt (e :: es) (fun (e :: es) -> T.SExpr(T.ECall(e, es)))
    | T.SExpr e ->
        reorder_stmt [e] (fun [e] -> T.SExpr e)
    | _ ->
        reorder_stmt [] (fun _ -> s)
  and do_expr e =
    match e with
    | T.EBinop(e1, op, e2) ->
        reorder_expr [e1; e2] (fun [e1; e2] -> T.EBinop(e1, op, e2))
    | T.EMem e ->
        reorder_expr [e] (fun [e] -> T.EMem e)
    | T.ESeq(s, e) -> 
        let s = do_stmt s in
        let (s', e) = do_expr e in
        s % s', e
    (* This case applies to standalone function calls. *)
    | T.ECall(e, es) -> 
        reorder_expr (e :: es) (fun (e :: es) -> T.ECall(e, es))
    | _ -> reorder_expr [] (fun _ -> e)
  in
  let rec linear s res = 
    match s with
    | T.SSeq(s1, s2) -> linear s1 (linear s2 res)
    | _ -> s :: res
  in
  linear (do_stmt s0) []

type bblock = T.stmt list

let basic_blocks (ss : T.stmt list) : bblock list * Temp.label = 
  let _ = print_endline "basic_blocks" in
  let ldone = Temp.new_label () in
  let rec blocks (ss : T.stmt list) (blist : bblock list) : bblock list = 
    match ss with
    | [] -> List.rev blist
    | (T.SLabel _) as head :: tail ->
        let rec next ss cur_block = 
          match ss with
          | (T.SJump(_, _) as s :: ss) | (T.SCJump(_, _, _, _, _) as s :: ss) ->
              blocks ss ((List.rev (s :: cur_block)) :: blist)
          | (T.SLabel l) :: _ -> next (T.SJump(T.ELabel l, [l]) :: ss) cur_block
          | s :: ss -> next ss (s :: cur_block)
          | [] -> next [T.SJump(T.ELabel ldone, [ldone])] cur_block
        in
        next tail [head]
    | _ -> blocks (T.SLabel (Temp.new_label ()) :: ss) blist
  in
  blocks ss [], ldone

let rec split_last ss = 
  match ss with
  | [s] -> [], s
  | s :: ss -> 
      let (ss', last) = split_last ss in
      s :: ss', last

let rec trace (table : bblock Symbol.table) (b : bblock) (todo : bblock list) : T.stmt list = 
  match b with
  | (T.SLabel l) :: _ ->
      let table = Symbol.insert table l [] in
      begin match split_last b with
      | most, T.SJump(T.ELabel l, _) ->
          begin match Symbol.lookup table l with
          | Some ((_::_) as b_next) ->
              most @ trace table b_next todo 
          | _ -> b @ (get_next table todo)
          end
      | most, T.SCJump(e1, op, e2, t, f) ->
          begin match Symbol.lookup table t, Symbol.lookup table f with
          | _, Some((_::_) as b_next) -> b @ trace table b_next todo
          | Some((_::_) as b_next), _ -> 
              most @ [T.SCJump(e1, Tree.not_rel op, e2, f, t)] @ trace table b_next todo
          | _ -> 
              let f' = Temp.new_label () in
              most @ [
                T.SCJump(e1, op, e2, t, f');
                T.SLabel f';
                T.SJump(T.ELabel f, [f])
              ] @ get_next table todo
          end
      | _, T.SJump(_, _) -> b @ get_next table todo
      end


and get_next (table : bblock Symbol.table) (todo : bblock list) : T.stmt list = 
  match todo with
  | [] -> []
  | (T.SLabel l :: _) :: todo -> 
      begin match Symbol.lookup table l with
      | Some ((_::_) as b) -> trace table b todo
      | _ -> get_next table todo
      end

let trace_schedule ((blocks, ldone) : bblock list * Temp.label) : T.stmt list = 
  let _ = print_endline "trace_schedule" in
  let prog = get_next (List.fold_left
    (fun acc ((T.SLabel l :: _) as b) ->
      Symbol.insert acc l b
    ) Symbol.empty blocks
  ) blocks
  in
  prog @ [T.SLabel ldone]

let canonize (s : Tree.stmt) : Tree.stmt list = 
  s |> linearize |> basic_blocks |> trace_schedule
