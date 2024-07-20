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

let word_size = 4

let fp = Temp.new_temp ()
let rv = Temp.new_temp ()

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
  T.ECall(T.ELabel(Temp.label_of_string fname), args)

let proc_entry_exit1 (frame : frame) (stmt : Tree.stmt) : Tree.stmt = 
  stmt

