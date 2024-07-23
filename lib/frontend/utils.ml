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

let show_tree_list trees = 
  (String.concat "\n" (List.map Tree.show_stmt trees))

let show_abstract_assembly instrs =
  Assem.show_instr_list string_of_temp instrs

let show_real_assembly instrs alloc = 
  let show_temp t = Temp.TempMap.find t alloc |> X86_frame.string_of_reg in
  Assem.show_instr_list show_temp instrs
