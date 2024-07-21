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

let _ = print_endline "-------AST------"
let _ = print_endline (Syntax.show_exp program)
let _ = print_endline ""

let _ = Semant.trans_prog program
let frags = Translate.get_fragments ()

let _ = List.iter
  (fun frag ->
    match frag with
    | X86_frame.Proc { body; frame } ->
        print_endline "-------FRAME------";
        print_endline (X86_frame.show_frame frame);
        print_endline "";

        print_endline "-------TREES------";
        List.iter (fun s -> print_endline (Tree.show_stmt s)) (Canon.canonize body);
        print_endline "";

        print_endline "-------ASSEMBLY------";
        let assembly = List.flatten (List.map (fun s -> Codegen.codegen s) (Canon.canonize body)) in
        print_endline (Assem.show_instr_list Utils.string_of_temp assembly);
        print_endline ""
    | X86_frame.String(_, _) as sf -> print_endline (X86_frame.show_fragment sf)
  ) frags

