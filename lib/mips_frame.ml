type access =
  | InFrame of int
  | InReg of Temp.temp

type frame = {
  label : Temp.label;
  formals : access list;
  locals_num : int ref;
}

let word_size = 4

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
