type temp =
  Temp of int
  [@@deriving show { with_path = false }]

type label = Symbol.symbol
  [@@deriving show { with_path = false }]

let new_temp = 
  let r = ref 0 in
  fun () -> incr r; Temp !r

let new_label = 
  let l = ref 0 in
  fun () -> incr l; Symbol.symbol ("L" ^ string_of_int !l)

let named_label str = 
  Symbol.symbol str
