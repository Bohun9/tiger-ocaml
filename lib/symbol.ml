type symbol =
  string * int

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

let counter = ref 0
let so_far : (int StringMap.t) ref = ref StringMap.empty

let symbol str = 
  match StringMap.find_opt str !so_far with
  | Some id -> str, id
  | None ->
      let id = incr counter; !counter in
      so_far := StringMap.add str id !so_far;
      str, id

let name (str, _) = str

type 'a table = 'a IntMap.t

let empty = IntMap.empty
let insert t (_, id) v = IntMap.add id v t
let lookup t (_, id) = IntMap.find_opt id t
let lookup_unsafe t (_, id)  = IntMap.find id t

let pp_symbol fmt (str,_) =
  Format.fprintf fmt "%s" str

let show_symbol (str,_) = str
