module T = Tree

type access =
  | InFrame of int
  | InReg of Temp.temp
  [@@deriving show]

type frame = {
  label : Temp.label;
  formals : access list;
  locals_num : int ref;
}
[@@deriving show]

type fragment = 
  | Proc of { body : Tree.stmt; frame : frame }
  | String of Temp.label * string
  [@@ deriving show]

let word_size = 8

let rax = Temp.new_temp ()
let rbx = Temp.new_temp ()
let rcx = Temp.new_temp ()
let rdx = Temp.new_temp ()

let rsi = Temp.new_temp ()
let rdi = Temp.new_temp ()
let rbp = Temp.new_temp ()
let rsp = Temp.new_temp ()

let r8 = Temp.new_temp ()
let r9 = Temp.new_temp ()
let r10 = Temp.new_temp ()
let r11 = Temp.new_temp ()
let r12 = Temp.new_temp ()
let r13 = Temp.new_temp ()
let r14 = Temp.new_temp ()
let r15 = Temp.new_temp ()

let special_regs = [rbp; rsp]
let callee_saved = [rbx; r12; r13; r14; r15;]
let caller_saved = [rax; rcx; rdx; rsi; rdi; r8; r9; r10; r11]

let call_trashed_regs = special_regs @ caller_saved
let args_regs = [rdi; rsi; rdx; rcx; r8; r9]

let fp = rbp
let rv = rax

let string_of_temp t = 
  match t with
  | _ when t = rax -> Some "%rax"
  | _ when t = rbx -> Some "%rbx"
  | _ when t = rcx -> Some "%rcx"
  | _ when t = rdx -> Some "%rdx"
  | _ when t = rsi -> Some "%rsi"
  | _ when t = rdi -> Some "%rdi"
  | _ when t = rbp -> Some "%rbp"
  | _ when t = rsp -> Some "%rsp"
  | _ when t = r8  -> Some "%r8"
  | _ when t = r9  -> Some "%r9"
  | _ when t = r10 -> Some "%r10"
  | _ when t = r11 -> Some "%r11"
  | _ when t = r12 -> Some "%r12"
  | _ when t = r13 -> Some "%r13"
  | _ when t = r14 -> Some "%r14"
  | _ when t = r15 -> Some "%r15"
  | _ -> None

let new_frame (label : Temp.label) (formals : bool list) : frame = 
  let locals_num = ref 0 in
  let formals = List.fold_left
    (fun acc _ ->
      acc @ [InFrame (incr locals_num; -word_size * !locals_num)]
    ) [] formals
  in
  { label = label; formals = formals; locals_num = locals_num }


let alloc_local (frm : frame) (_ : bool) : access =
  incr (frm.locals_num);
  InFrame (-word_size * !(frm.locals_num))

let formals frm = frm.formals
let label frm = frm.label

let exp (access : access) (fp : Tree.expr) : Tree.expr = 
  match access with
  | InFrame off -> T.EMem(T.EBinop(fp, T.ADD, T.EConst off))
  | InReg t -> T.ETemp t

let external_call (fname : string) (args : T.expr list) : T.expr =
  T.ECall(T.ELabel(Temp.named_label fname), args)

let proc_entry_exit1 (_ : frame) (stmt : Tree.stmt) : Tree.stmt = 
  stmt

