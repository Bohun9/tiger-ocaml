open Frontend
open Backend
open Utils

let usage_msg = ""
let input_file = ref None
let output_file = ref None
let debug = ref false

let anon_fun f = input_file := Some f
let speclist =
  [ ("--debug", Arg.Set debug, "Show all intermediate languages");
    ("-o", Arg.String (fun s -> output_file := Some s), "Save the output to a file")
  ]

let _ = Arg.parse speclist anon_fun usage_msg
let file = 
  match !input_file with
  | Some f -> f
  | None -> 
      Printf.eprintf "error: the file was not provided\n";
      exit 1

let debug phase s = 
  if !debug then (
    Printf.eprintf "---------- %s ---------\n" phase;
    Printf.eprintf "%s\n" s;
    Printf.eprintf "\n";
    flush stderr
  ) else
    ()

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

let source = read_file file
let program = parse_from_string source
let _ = Find_escape.find_escape program
let _ = debug "AST" (Syntax.show_exp program)

let _ = Semant.trans_prog program
let frags = Translate.get_fragments ()

let functions, strings = List.fold_left
  (fun (fs, ss) frag ->
    match frag with
    | X86_frame.Proc { body; frame } ->
        let _ = debug "FRAME" (X86_frame.show_frame frame) in
        let trees = Canon.canonize body in
        let _ = debug "CANONIZED TREES" (show_tree_list trees) in
        let abstract_assembly = Codegen.codegen trees in
        let _ = debug "ABSTRACT ASSEMBLY" (show_abstract_assembly abstract_assembly) in
        let assembly, allocation = Register_allocation.alloc abstract_assembly frame in
        let assembly_str = show_real_assembly assembly allocation in
        let _ = debug "X86 ASSEMBLY" assembly_str in
        assembly_str :: fs, ss 
    | X86_frame.String(l, s) as sf ->
        fs, X86_frame.string l s :: ss
  ) ([], []) frags

let _ = debug "FINAL PROGRAM" ""

let header = ".globl tigermain\n"
let data = ".section .data\n" ^ String.concat "" strings
let text = ".section .text\n" ^ String.concat "" functions
let final_program = header ^ data ^ text

let _ = 
  match !output_file with
  | None -> print_string final_program
  | Some f ->
      let ch = open_out f in
      Printf.fprintf ch "%s" final_program;
      close_out ch
