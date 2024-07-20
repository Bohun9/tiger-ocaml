open Tiger_ml
open Utils

let read_file file =
  let ch =
    try open_in_bin file
    with Sys_error s ->
      Printf.eprintf "%s\n" s;
      exit 1
  in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let file = Sys.argv.(1)
let source = read_file file
let program = parse_from_string source

(* let _ = print_endline (Syntax.show_exp program) *)

let _ = Semant.trans_prog program
let frags = Translate.get_fragments ()

let _ = List.iter
  (fun frag ->
    match frag with
    | Mips_frame.Proc { body; frame } ->
        print_endline (Mips_frame.show_frame frame);
        List.iter (fun s -> print_endline (Tree.show_stmt s)) (Canon.canonize body)
    | _ -> print_endline (Mips_frame.show_fragment frag)
  ) frags

