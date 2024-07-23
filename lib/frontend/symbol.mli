(* This module provides efficient mappings based on symbols.
   The mapping between strings and symbols via the functions
   `name` and `symbols` is a bijection.
 *)

type symbol

type 'a table

val symbol : string -> symbol
val name : symbol -> string

val empty : 'a table
val insert : 'a table -> symbol -> 'a -> 'a table
val lookup : 'a table -> symbol -> 'a option
val lookup_unsafe : 'a table -> symbol -> 'a

val pp_symbol : Format.formatter -> symbol -> unit
val show_symbol : symbol -> string
