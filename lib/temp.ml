type temp =
  Temp of int
  [@@deriving show]

type label =
  Label of string
  [@@deriving show]

let new_temp = 
  let r = ref 0 in
  fun () -> incr r; Temp !r

let label_of_string s = Label s

let new_label = 
  let l = ref 0 in
  fun () -> incr l; Label ("L" ^ string_of_int !l)
