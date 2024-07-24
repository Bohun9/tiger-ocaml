module T = Tree

type access =
  | InFrame of int
  | InReg of Temp.temp
  [@@deriving show { with_path = false}]

type frame = {
  label : Temp.label;
  formals : access list;
  locals_num : int ref;
  max_args_call : int ref;
}
[@@deriving show { with_path = false}]

type fragment = 
  | Proc of { body : Tree.stmt; frame : frame }
  | String of Temp.label * string
  [@@ deriving show { with_path = false}]

type register =
  | Rax
  | Rbx
  | Rcx
  | Rdx
  | Rsi
  | Rdi
  | Rbp
  | Rsp
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  [@@ deriving show { with_path = false}]

module RegSet = Set.Make(struct 
  type t = register
  let compare = compare
end)

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

let string_of_reg r = 
  match r with
  | Rax -> "%rax"
  | Rbx -> "%rbx"
  | Rcx -> "%rcx"
  | Rdx -> "%rdx"
  | Rsi -> "%rsi"
  | Rdi -> "%rdi"
  | Rbp -> "%rbp"
  | Rsp -> "%rsp"
  | R8  -> "%r8"
  | R9  -> "%r9"
  | R10 -> "%r10"
  | R11 -> "%r11"
  | R12 -> "%r12"
  | R13 -> "%r13"
  | R14 -> "%r14"
  | R15 -> "%r15"

let temp_to_reg_mapping = Temp.TempMap.of_list [
  rax, Rax;
  rbx, Rbx;
  rcx, Rcx;
  rdx, Rdx;
  rsi, Rsi;
  rdi, Rdi;
  rbp, Rbp;
  rsp, Rsp;
  r8 , R8 ;
  r9 , R9 ;
  r10, R10;
  r11, R11;
  r12, R12;
  r13, R13;
  r14, R14;
  r15, R15;
]

let alloc_local (frm : frame) (escape : bool) : access =
  if escape then (
    incr (frm.locals_num);
    InFrame (-word_size * !(frm.locals_num))
  ) else
    InReg (Temp.new_temp ())

let new_frame (label : Temp.label) (formals : bool list) : frame = 
  let locals_num = ref 0 in
  let frame = { label = label; formals = []; locals_num = ref 0; max_args_call = ref 0 } in
  let formals = List.fold_left
    (fun acc esc ->
      acc @ [alloc_local frame esc]
    ) [] formals
  in
  { frame with formals = formals }
  
let register_function_call (frame : frame) (args_num : int) : unit = 
  frame.max_args_call := max !(frame.max_args_call) args_num

let formals frm = frm.formals
let label frm = frm.label

let exp (access : access) (fp : Tree.expr) : Tree.expr = 
  match access with
  | InFrame off -> T.EMem(T.EBinop(fp, T.ADD, T.EConst off))
  | InReg t -> T.ETemp t

let external_call (fname : string) (args : T.expr list) : T.expr =
  T.ECall(T.ELabel(Temp.named_label fname), args)

let string (label : Temp.label) (str : string) : string = 
  let ascii = String.escaped str in
  let length = String.length ascii in
  Printf.sprintf "%s:\n    .quad %d\n    .ascii \"%s\"\n" (Temp.show_label label) length ascii 

  (* [proc_entry_exit1] adds a code for saving and restoring callee-saved registers.
     It also moves function arguments to proper temporaries. *)
let proc_entry_exit1 (frame : frame) (stmt : Tree.stmt) : Tree.stmt = 
  let move_args =
    List.mapi (fun i access ->
      let src = 
        if i < 6 then
          T.ETemp (List.nth args_regs i)
        else
          let j = i - 6 in
          T.EMem(T.EBinop(T.ETemp fp, T.ADD, T.EConst ((j + 2) * word_size)))
      in
      Tree.SMove(exp access (T.ETemp fp), src)
    ) frame.formals
  in
  let prologue, epilog = 
    List.fold_right (fun r (p, e) ->
      let t = Temp.new_temp () in
      let p = Tree.SMove(Tree.ETemp t, Tree.ETemp r) :: p in
      let e = Tree.SMove(Tree.ETemp r, Tree.ETemp t) :: e in
      (p, e)
    ) callee_saved ([], [])
  in
  Tree.seq ([Tree.SLabel frame.label; Tree.seq prologue; Tree.seq move_args; stmt; Tree.seq epilog])

(* [proc_entry_exit2] adds a dummy instruction that forces special registers to be live at the end. *)
let proc_entry_exit2 (code : Assem.instr list) : Assem.instr list = 
  code @ [ Assem.IOper { assem = ""; dst = []; src = rax :: special_regs @ callee_saved; jump = None } ]

(* [proc_entry_exit3] handles stack pointers registers.  *)
let proc_entry_exit3 (frame : frame) (code : Assem.instr list) : Assem.instr list = 
  let frame_size = word_size * (!(frame.locals_num) + (max 0 !(frame.max_args_call))) in
  let frame_size = frame_size + (16 - (frame_size mod 16)) mod 16 in
  let (label :: code) = code in
  let prologue =
    [ Assem.IOper { assem = "push %rbp\n"; src = []; dst = []; jump = None };
      Assem.IOper { assem = "movq %rsp, %rbp\n"; src = []; dst = []; jump = None };
    ]
  in
  let prologue_frame, epilogue_frame = 
    if frame_size > 0 then
      [ Assem.IOper { assem = Printf.sprintf "subq $%s, %%rsp\n" (string_of_int frame_size); src = []; dst = []; jump = None } ],
      [ Assem.IOper { assem = Printf.sprintf "addq $%s, %%rsp\n" (string_of_int frame_size); src = []; dst = []; jump = None } ]
    else
      [], []
  in
  let epilogue = 
    [ Assem.IOper { assem = "pop %rbp\n"; src = []; dst = []; jump = None };
      Assem.IOper { assem = "ret\n"; src = []; dst = []; jump = None };
    ]
  in
  label :: prologue @ prologue_frame @ code @ epilogue_frame @ epilogue

