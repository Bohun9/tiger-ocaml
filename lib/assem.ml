(* The abstract assembly that the `Codegen` module will produce
   from the `Tree` representation. At this point, we don't need
   the exact form of the instructions, only their sources, destinations
   and possible jump targets.
 *)

type instr =
  | IOper of {
      assem: string;
      dst: Temp.temp list;
      src: Temp.temp list;
      jump: Temp.label list option;
    }
  | ILabel of {
      assem: string;
      lab: Temp.label;
    }
  | IMove of {
      assem: string;
      dst: Temp.temp;
      src: Temp.temp;
    }
  [@@deriving show { with_path = false }]

let format (say_temp : Temp.temp -> string) (i : instr) : string = 
  let speak assem dst src jmp = 
    let say_lab = Symbol.name in
    let rec f xs =
      match xs with
      | [] -> ""
      | '`' :: 's' :: i :: xs -> say_temp (List.nth src (Char.code i - Char.code '0')) ^ f xs
      | '`' :: 'd' :: i :: xs -> say_temp (List.nth dst (Char.code i - Char.code '0')) ^ f xs
      | '`' :: 'j' :: i :: xs -> say_lab  (List.nth jmp (Char.code i - Char.code '0')) ^ f xs
      | '`' :: '`' :: xs -> "`" ^ f xs
      | x :: xs -> String.make 1 x ^ f xs
    in
    f (List.of_seq (String.to_seq assem))
  in
  let indent = String.make 2 ' ' in
  match i with
  | IOper { assem; dst; src; jump = None } -> indent ^ speak assem dst src []
  | IOper { assem; dst; src; jump = Some jmp } -> indent ^ speak assem dst src jmp
  | ILabel { assem; _ } -> assem
  | IMove { assem; dst; src } -> indent ^ speak assem [dst] [src] []

let show_instr_list (say_temp : Temp.temp -> string) is = 
  "[\n" ^ (String.concat "\n" (List.map (format say_temp) is)) ^ "\n]"
