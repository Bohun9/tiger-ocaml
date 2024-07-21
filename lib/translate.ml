module Frame = X86_frame
module T = Tree

type level = {
  frame : Frame.frame;
  parent : level option;
  unique : unit ref;
}

and access = level * Frame.access

let fragments = ref []

let get_fragments () = !fragments

let eq_level lvl1 lvl2 = 
  lvl1.unique == lvl2.unique

let outermost =
  { frame = Frame.new_frame (Temp.named_label "main") []
  ; parent = None
  ; unique = ref ()
  }

let new_level (parent : level) (label : Temp.label) (formals : bool list) : level = 
  { frame = Frame.new_frame label (true :: formals)
  ; parent = Some parent
  ; unique = ref ()
  }

let alloc_local (lvl : level) (esc : bool) : access = 
  lvl, Frame.alloc_local lvl.frame esc

let formals (lvl : level) : access list = 
  List.map (fun x -> lvl, x) (List.tl lvl.frame.formals)

let static_link (lvl : level) : Frame.access = 
  List.hd lvl.frame.formals

type exp = 
  | Ex of Tree.expr
  | Nx of Tree.stmt
  | Cx of (Temp.label -> Temp.label -> Tree.stmt)
  [@@deriving show { with_path = false } ]

let rec seq (ss : Tree.stmt list) : Tree.stmt = 
  match ss with
  | [] -> T.SExpr(T.EConst 0)
  | [s] -> s
  | s :: ss -> Tree.SSeq(s, seq ss)

let un_ex (e : exp) : Tree.expr = 
  match e with
  | Ex e -> e
  | Nx s -> Tree.ESeq(s, EConst 0)
  | Cx genstmt ->
      let r = Temp.new_temp () in
      let t = Temp.new_label () in
      let f = Temp.new_label () in
      T.ESeq(
        seq [
          T.SMove(T.ETemp r, T.EConst 0);
          genstmt t f;
          T.SLabel t;
          T.SMove(T.ETemp r, T.EConst 1);
          T.SLabel f
        ]
        , T.ETemp r)

let un_nx (e : exp) : Tree.stmt = 
  match e with
  | Ex e -> SExpr e
  | Nx s -> s
  | Cx genstmt ->
      let t = Temp.new_label () in
      let f = Temp.new_label () in
      seq [
        genstmt t f;
        T.SLabel t;
        T.SLabel f
      ]

let un_cx (e : exp) : (Temp.label -> Temp.label -> Tree.stmt) =
  match e with
  | Ex e -> fun t f -> SCJump(e, NEQ, T.EConst 0, t, f)
  | Nx _ -> failwith "un_cx(Nx ...) called"
  | Cx c -> c

let simple_var (def_lvl, frame_access : access) (use_lvl : level) : exp = 
  let rec aux lvl fp =
    if eq_level def_lvl lvl then
      Ex(Frame.exp frame_access fp)
    else
      match lvl.parent with
      | Some parent -> aux parent (Frame.exp (static_link lvl) fp)
      | None -> failwith "simple_var: internal error"
  in
  aux use_lvl (ETemp Frame.fp)

let field_var (rcd : exp) (off : int) : exp = 
  Ex(T.EMem(T.EBinop(un_ex rcd, ADD, T.EBinop(T.EConst off, MUL, T.EConst Frame.word_size))))

let subscript_var (arr : exp) (idx : exp) : exp = 
  Ex(T.EMem(T.EBinop(un_ex arr, ADD, T.EBinop(un_ex idx, MUL, T.EConst Frame.word_size))))

let nil_exp = Ex(T.EConst 0)

let int_exp (i : int) : exp = Ex(T.EConst i)

let op_exp (e1 : exp) (op : Syntax.op) (e2 : exp) : exp = 
  match op with
  | OpAdd | OpSub | OpMul | OpDiv ->
      let op =
        begin match op with
        | OpAdd -> T.ADD
        | OpSub -> T.SUB
        | OpMul -> T.MUL
        | OpDiv -> T.DIV
        | _ -> failwith ""
        end
      in
      Ex(T.EBinop(un_ex e1, op, un_ex e2))
  | OpEq | OpNeq | OpLt | OpLe | OpGt | OpGe ->
      let op =
        begin match op with
        | OpEq -> T.EQ
        | OpNeq -> T.NEQ
        | OpLt -> T.LT
        | OpLe -> T.LE
        | OpGt -> T.GT
        | OpGe -> T.GE
        | _ -> failwith ""
        end
      in
      Cx(fun t f -> T.SCJump(un_ex e1, op, un_ex e2, t, f))

let nop : exp = Ex(T.EConst 0)

let seq_exp (e1 : exp) (e2 : exp) : exp = 
  Ex(T.ESeq(un_nx e1, un_ex e2))

let assign_exp (var : exp) (e : exp) : exp = 
  Nx(T.SMove(un_ex var, un_ex e))

let if_exp (e1 : exp) (e2 : exp) (e3 : exp) : exp = 
  let r = Temp.new_temp () in
  let t = Temp.new_label () in
  let f = Temp.new_label () in
  let e = Temp.new_label () in
  Ex(T.ESeq(seq[
    un_cx e1 t f;
    T.SLabel t;
    T.SMove(T.ETemp r, un_ex e2);
    T.SJump(T.ELabel e, [e]);
    T.SLabel f;
    T.SMove(T.ETemp r, un_ex e3);
    T.SLabel e;
    ], ETemp r
  ))

let string_exp (str : string) : exp = 
  let l = Temp.new_label () in
  fragments := Frame.String(l, str) :: !fragments;
  Ex(T.ELabel l)

let record_exp (es : exp list) : exp = 
  let r = Temp.new_temp () in
  Ex(
    T.ESeq(
      T.SSeq(
        T.SMove(T.ETemp r, Frame.external_call "malloc" [T.EConst (Frame.word_size * List.length es)])
        , seq (List.mapi
            (fun i e -> T.SMove(T.EMem(T.EBinop(T.ETemp r, ADD, T.EConst (Frame.word_size * i))), un_ex e))
          es)
      )
    , T.ETemp r)
  )

let array_exp (size : exp) (init : exp) : exp = 
  let a = Temp.new_temp () in
  Ex(
    T.ESeq(
      T.SMove(T.ETemp a, Frame.external_call "init_array" [un_ex size; un_ex init])
    , T.ETemp a)
  )

let while_exp (test : exp) (body : exp) (t : Temp.label) : exp = 
  let b = Temp.new_label () in
  let d = Temp.new_label () in
  Nx(seq[
    T.SLabel t;
    un_cx test b d;
    T.SLabel b;
    un_nx body;
    T.SJump(T.ELabel t, [t]);
    T.SLabel d;
  ])

let break_exp (ldone : Temp.label) : exp = 
  Nx(T.SJump(T.ELabel ldone, [ldone]))

let call_exp (fn_label : Temp.label) (fn_lvl : level) (lvl : level) (es : exp list) : exp = 
  let fn_parent_lvl = 
    match fn_lvl.parent with
    | Some l -> l
    | _ -> failwith "call_exp: internal error 1"
  in
  let rec aux lvl fp = 
    if eq_level fn_parent_lvl lvl then
      fp
    else
      let parent_fp = Frame.exp (static_link lvl) fp in
      match lvl.parent with
      | Some parent -> aux parent parent_fp
      | None -> failwith "call_exp: internal error 2"
  in
  let link = aux lvl (T.ETemp Frame.fp) in
  Ex(T.ECall(T.ELabel fn_label, link :: List.map (fun e -> un_ex e) es))

let let_exp (es : exp list) (body : exp) : exp = 
  Ex(
    T.ESeq(
      seq (List.map (fun e -> un_nx e) es)
      , un_ex body
    )
  )

let var_dec (_, frame_access : access) (e : exp) : exp = 
  Nx(T.SMove(Frame.exp frame_access (T.ETemp Frame.fp), un_ex e))

let proc_entry_exit (level : level) (body : exp) : unit = 
  let ret = T.SMove(T.ETemp Frame.rv, un_ex body) in
  fragments := Frame.Proc { frame = level.frame; body = Frame.proc_entry_exit1 level.frame ret } :: !fragments

