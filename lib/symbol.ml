type symbol =
  string
  [@@deriving show]

module SymbolMap = Map.Make(struct
  type t = symbol
  let compare = compare
end)

type 'a table = 'a SymbolMap.t

let empty = SymbolMap.empty
let insert t k v = SymbolMap.add k v t
let lookup t k = SymbolMap.find_opt k t
let lookup_unsafe t k  = SymbolMap.find k t
