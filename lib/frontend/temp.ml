type temp =
  Temp of int
  [@@deriving show { with_path = false }]

type label = Symbol.symbol
  [@@deriving show { with_path = false }]

let string_of_temp (Temp i) = "t" ^ (string_of_int i)

let temp_counter = ref 0

let new_temp = 
  fun () -> incr temp_counter; Temp !temp_counter

let created_temps () = 
  let rec aux n = 
    if n > !temp_counter then
      []
    else
      Temp n :: aux (n + 1)
  in
  aux 1

let new_label = 
  let c = ref 0 in
  fun () -> incr c; Symbol.symbol ("L" ^ string_of_int !c)

let named_label str = 
  Symbol.symbol str

module TempSet = Set.Make(struct 
  type t = temp
  let compare = compare
end)

module TempMap = Map.Make(struct 
  type t = temp
  let compare = compare
end)
