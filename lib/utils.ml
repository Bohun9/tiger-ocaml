open Syntax

let parse_from_string (source : string) : exp =
  let lexbuf = Lexing.from_string source in
  try Parser.file Lexer.token lexbuf
  with Parser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!"
      (Lexing.lexeme_start lexbuf);
    failwith ""

let string_of_temp t = 
  match X86_frame.string_of_temp t with
  | Some s -> s
  | None -> Temp.string_of_temp t
