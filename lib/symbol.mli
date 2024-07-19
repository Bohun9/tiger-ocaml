type symbol = string
  [@@deriving show]

type 'a table

val empty : 'a table
val insert : 'a table -> symbol -> 'a -> 'a table
val lookup : 'a table -> symbol -> 'a option
val lookup_unsafe : 'a table -> symbol -> 'a
