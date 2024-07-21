type temp

type label = Symbol.symbol
  [@@deriving show { with_path = false }]

val new_temp : unit -> temp
val new_label : unit -> label
val named_label : string -> label

val string_of_temp : temp -> string

val pp_temp : Format.formatter -> temp -> unit
val show_temp : temp -> string
