open Frontend
module A = Assem
module T = Tree
module F = X86_frame

let move ~dst ~src =
  A.IMove {
    assem = "movq `s0, `d0\n";
    dst = dst; src = src
  }

let codegen_one (stmt : Tree.stmt) : A.instr list = 
  let code = ref [] in
  let emit i = code := i :: !code in
  let emits is = code := (List.rev is) @ !code in
  let result k = 
    let t = Temp.new_temp () in
    k t; t
  in
  let rec munch_expr (e : Tree.expr) : Temp.temp = 
    match e with
    | T.EConst i ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq $%s, `d0\n" (string_of_int i);
              dst = [r]; src = []; jump = None;
            }
          )
        )
    | T.ELabel l -> 
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq $%s, `d0\n" (Temp.show_label l);
              dst = [r]; src = []; jump = None;
            }
          )
        )
    | T.ETemp t -> t
    | T.EBinop(T.EConst i, ADD, e1) ->
        munch_expr (T.EBinop(e1, ADD, T.EConst i))
    | T.EBinop(e1, ADD, T.EConst i) ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "leaq %s(`s0), `d0\n" (string_of_int i);
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
                | _ -> failwith ""
              in
              emits [
                move ~dst:r ~src:t1;
                A.IOper {
                  assem = Printf.sprintf "%s `s0, `d0\n" op_name;
                  dst = [r]; src = [t2; r]; jump = None
                };
              ]
          | T.DIV ->
              emits [
                A.IOper {
                  assem = "movq $0, `d0\n";
                  dst = [F.rdx]; src = []; jump = None
                };
                move ~dst:F.rax ~src:t1;
                A.IOper {
                  assem = "idivq `s2\n";
                  dst = [F.rdx; F.rax]; src = [F.rdx; F.rax; t2]; jump = None
                };
                move ~dst:r ~src:F.rax;
              ]
        )
    | T.EMem(T.EBinop(e1, ADD, T.EBinop(e2, MUL, T.EConst i))) when List.mem i [1;2;4;8] ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq (`s0, `s1, %s), `d0\n" (string_of_int i);
              dst = [r]; src = [munch_expr e1; munch_expr e2]; jump = None
            }
          )
        )
    | T.EMem(T.EBinop(e1, T.ADD, T.EConst i)) ->
        result (fun r -> emit (
            A.IOper {
              assem = Printf.sprintf "movq %s(`s0), `d0\n" (string_of_int i);
              dst = [r]; src = [munch_expr e1]; jump = None
            }
          )
        )
    | T.EMem e ->
        result (fun r -> emit (
            A.IOper {
              assem = "movq (`s0), `d0\n";
              dst = [r]; src = [munch_expr e]; jump = None
            }
          )
        )
    | T.ECall(_, _) -> failwith "ECall"
    | T.ESeq(_, _) -> failwith "ESeq"
  in
  let rec munch_args args i =
    match args with
    | [] -> []
    | a :: args ->
        if i < 6 then
          let r = List.nth X86_frame.args_regs i in
          munch_stmt (T.SMove(T.ETemp r, a));
          r :: munch_args args (i + 1)
        else
          let j = i - 6 in
          munch_stmt (T.SMove(T.EMem(T.EBinop(T.ETemp F.rsp, T.ADD, T.EConst (F.word_size * j))), a));
          munch_args args (i + 1)
  and munch_stmt (s : Tree.stmt) : unit = 
    match s with
    | T.SMove(T.ETemp t, T.ECall(T.ELabel l, args)) ->
        emits [
          A.IOper {
            assem = Printf.sprintf "call %s\n" (Symbol.name l);
            dst = X86_frame.call_trashed_regs; src = munch_args(args) 0; jump = None
          };
          move ~dst:t ~src: X86_frame.rax
        ]
    | T.SMove(T.ETemp t1, e) ->
        emit (move ~dst:t1 ~src:(munch_expr e))
    (* In general the register allocator can remove many moves between temporaries, but it does
       not cope with optimizing moves with constants to memory, so we must treat this case special here.
     *)
    | T.SMove(T.EMem(T.EBinop(e1, ADD, T.EConst i1)), T.EConst i2) ->
        let t1 = munch_expr e1 in
        emit (
          A.IOper {
            assem = Printf.sprintf "movq $%d, %d(`s0)\n" i2 i1;
            dst = []; src = [t1]; jump = None
          }
        )
    | T.SMove(T.EMem(T.EBinop(e1, ADD, T.EConst i)), e2) ->
        let t1 = munch_expr e1 in
        let t2 = munch_expr e2 in
        emit (
          A.IOper {
            assem = Printf.sprintf "movq `s1, %d(`s0)\n" i;
            dst = []; src = [t1; t2]; jump = None
          }
        )
    | T.SMove(T.EMem e1, e2) ->
        let t1 = munch_expr e1 in
        let t2 = munch_expr e2 in
        emit (
          A.IOper {
            assem = "movq `s0, (`s1)\n";
            dst = []; src = [t2; t1]; jump = None
          }
        )
    | T.SMove(_, _) -> failwith ""
    | T.SExpr(T.ECall(T.ELabel l, args)) ->
        emit (
          A.IOper {
            assem = Printf.sprintf "call %s\n" (Symbol.name l);
            dst = X86_frame.call_trashed_regs; src = munch_args(args) 0; jump = None
          }
        )
    | T.SExpr (T.ETemp _) -> ()
    | T.SExpr _ -> failwith "SExpr"
    | T.SJump(T.ELabel l, [l']) when l = l' -> 
        emit (
          A.IOper {
            assem = Printf.sprintf "jmp %s\n" (Symbol.name l);
            dst = []; src = []; jump = Some([l])
          }
        )
    | T.SJump(_, _) -> failwith "SJump"
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
            assem = Printf.sprintf "cmpq `s0, `s1\n";
            dst = []; src = [t1; t2]; jump = None
          };
          A.IOper {
            assem = Printf.sprintf "%s `j0\n" jmp_name;
            dst = []; src = []; jump = Some [t; f]
          }
        ]
    | T.SSeq(_, _) -> failwith "SSeq"
    | T.SLabel l -> emit (
          A.ILabel {
            assem = Symbol.name l ^ ":\n";
            lab = l
          }
        )
  in
  munch_stmt stmt;
  List.rev !code

let codegen (t : Tree.stmt list) : Assem.instr list = 
  let code = List.flatten (List.map codegen_one t) in
  F.proc_entry_exit2 code

