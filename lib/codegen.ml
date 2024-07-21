module A = Assem
module T = Tree
module F = X86_frame

exception CodegenError of string

let internal_error fn_name ctor_name =
  raise (CodegenError (Printf.sprintf "this place should not have been reached %s(%s)" fn_name ctor_name))

let error msg = 
  raise (CodegenError msg)

let move ~dst ~src =
  A.IMove {
    assem = "movq `s0, `d0";
    dst = dst; src = src
  }

let codegen (stmt : Tree.stmt) : A.instr list = 
  let code = ref [] in
  let emit i = code := i :: !code in
  let emits is = code := (List.rev is) @ !code in
  let result k = 
    let t = Temp.new_temp () in
    k t; t
  in
  let rec munch_expr (e : Tree.expr) : Temp.temp = 
    let internal_error = internal_error "munch_expr" in
    match e with
    | T.EConst i ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq $%s, `d0" (string_of_int i);
              dst = [r]; src = []; jump = None;
            }
          )
        )
    | T.ELabel _ -> error "ELabel"
    | T.ETemp t -> t
    | T.EBinop(T.EConst i, ADD, e1) ->
        munch_expr (T.EBinop(e1, ADD, T.EConst i))
    | T.EBinop(e1, ADD, T.EConst i) ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "leaq %s(`s0), `d0" (string_of_int i);
              dst = [r]; src = [munch_expr e1]; jump = None
            }
          )
        )
    | T.EBinop(e1, op, e2) ->
        let t1 = munch_expr e1 in
        let t2 = munch_expr e2 in
        result (fun r -> 
          match op with
          | T.ADD | T.SUB | T.MUL ->
              let op_name = 
                match op with
                | T.ADD -> "addq"
                | T.SUB -> "subq"
                | T.MUL -> "imulq"
                | _ -> error ""
              in
              emits [
                move ~dst:r ~src:t1;
                A.IOper {
                  assem = Printf.sprintf "%s `s0, `d0" op_name;
                  dst = [r]; src = [t2; r]; jump = None
                };
              ]
          | T.DIV ->
              (* The `idiv r` instruction caculates rdx:rax / r.
                 rax <- quotient
                 rdx <- remainder

                 r <- t1 / t2

                 rdx <- 0
                 rax <- t1
                 idiv t2
                 r <- rax
               *)
              emits [
                A.IOper {
                  assem = "movq $0, `d0";
                  dst = [F.rdx]; src = []; jump = None
                };
                move ~dst:F.rax ~src:t1;
                A.IOper {
                  assem = "idivq `s0";
                  dst = [F.rbp; F.rax]; src = [F.rbp; F.rax; t2]; jump = None
                };
                move ~dst:r ~src:F.rax;
              ]
        )
    | T.EMem(T.EBinop(e1, ADD, T.EBinop(T.EConst i, MUL, e2))) when List.mem i [1;2;4;8] ->
        munch_expr (T.EMem(T.EBinop(e1, ADD, T.EBinop(e2, MUL, T.EConst i))))
    | T.EMem(T.EBinop(e1, ADD, T.EBinop(e2, MUL, T.EConst i))) when List.mem i [1;2;4;8] ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq (`s0, `s1, %s), `d0" (string_of_int i);
              dst = [r]; src = [munch_expr e1; munch_expr e2]; jump = None
            }
          )
        )
    | T.EMem(T.EBinop(T.EConst i, T.ADD, e1)) ->
        munch_expr (T.EMem(T.EBinop(e1, T.ADD, T.EConst i)))
    | T.EMem(T.EBinop(e1, T.ADD, T.EConst i)) ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq %s(`s0), `d0" (string_of_int i);
              dst = [r]; src = [munch_expr e1]; jump = None
            }
          )
        )
    | T.EMem e ->
        result (fun r -> emit (
            A.IOper {
              assem = "movq (`s0), `d0";
              dst = [r]; src = [munch_expr e]; jump = None
            }
          )
        )
    | T.ECall(_, _) -> error "ECall"
    | T.ESeq(_, _) -> error "ESeq"
  in
  let rec munch_args args i =
    match args with
    | [] -> []
    | a :: args ->
        let r =
          begin match List.nth_opt X86_frame.args_regs i with
          | Some r -> r
          | None -> error "the function has more that 6 arguments"
          end
        in
        munch_stmt (T.SMove(T.ETemp r, a));
        r :: munch_args args (i + 1)
  and munch_stmt (s : Tree.stmt) : unit = 
    let internal_error = internal_error "munch_stmt" in
    match s with
    | T.SMove(T.ETemp t, T.ECall(T.ELabel l, args)) ->
        emits [
          A.IOper {
            assem = Printf.sprintf "call %s" (Symbol.name l);
            dst = X86_frame.call_trashed_regs; src = munch_args(args) 0; jump = None
          };
          move ~dst:t ~src: X86_frame.rax
        ]
    | T.SMove(T.ETemp r, e) -> emit (
          A.IOper {
            assem = "movq `s0, `d0";
            dst = [r]; src = [munch_expr e]; jump = None
          }
        )
    | T.SMove(T.EMem e1, e2) ->
        let t1 = munch_expr e1 in
        let t2 = munch_expr e2 in
        emit (
          A.IOper {
            assem = "movq `s0, (`s1)";
            dst = []; src = [t2; t1]; jump = None
          }
        )
    | T.SMove(_, _) -> error ""
    (* `T.SExpr(T.EConst 0)`, which represents a no-op, should have been eliminated by the `Canon` module.
       Only the `T.SExpr(T.ECall(...))` form is allowed here.
     *)
    | T.SExpr(T.ECall(T.ELabel l, args)) ->
        emit (
          A.IOper {
            assem = Printf.sprintf "call %s" (Symbol.name l);
            dst = X86_frame.call_trashed_regs; src = munch_args(args) 0; jump = None
          }
        )
    | T.SExpr _ -> error "SExpr"
    | T.SJump(T.ELabel l, [l']) when l = l' -> 
        emit (
          A.IOper {
            assem = Printf.sprintf "jmp %s" (Symbol.name l);
            dst = []; src = []; jump = Some([l])
          }
        )
    | T.SJump(_, _) -> error "SJump"
    | T.SCJump(e1, op, e2, t, f) ->
        let t1 = munch_expr e1 in
        let t2 = munch_expr e2 in
        let jmp_name = match op with
        | EQ -> "je"
        | NEQ -> "jne"
        | LT -> "jg"
        | LE -> "jge"
        | GT -> "jl"
        | GE -> "jle"
        in
        emits [
          A.IOper {
            assem = Printf.sprintf "cmpq `s0, `s1";
            dst = []; src = [t1; t2]; jump = None
          };
          A.IOper {
            assem = Printf.sprintf "%s `j0" jmp_name;
            dst = []; src = []; jump = Some [t; f]
          }
        ]
    | T.SSeq(_, _) -> error "SSeq"
    | T.SLabel l -> emit (
          A.ILabel {
            assem = Symbol.name l ^ ":";
            lab = l
          }
        )
  in
  munch_stmt stmt;
  List.rev !code
