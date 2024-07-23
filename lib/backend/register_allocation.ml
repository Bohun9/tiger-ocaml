open Frontend
module A = Assem
module F = X86_frame
module T = Tree
module TempSet = Temp.TempSet
module TempMap = Temp.TempMap

type allocation = F.register TempMap.t

let remove_dummy_moves (code : A.instr list) (allocation : allocation) : A.instr list = 
  List.filter (fun i ->
    match i with
    | A.IMove { dst; src } ->
        let r1 = TempMap.find dst allocation in
        let r2 = TempMap.find src allocation in
        r1 != r2
    | _ -> true
  ) code

let handle_spill (code : A.instr list) (frame : F.frame) (group : Temp.temp list) : A.instr list = 
  let access = F.alloc_local frame true in
  List.flatten (List.map (fun i ->
    let dst, src =
      match i with
      | A.ILabel _ -> [], []
      | A.IMove { dst; src } -> [dst], [src]
      | A.IOper { dst; src; _ } -> dst, src
    in
    let intersect l1 l2 = not (TempSet.is_empty (TempSet.inter (TempSet.of_list l1) (TempSet.of_list l2))) in
    let replace l t = List.map (fun t' -> if List.mem t' group then t else t') l in
    let src, c1 = 
      if intersect src group then
        let t = Temp.new_temp () in
        replace src t, Codegen.codegen_one (T.SMove(T.ETemp t, F.exp access (T.ETemp F.fp)))
      else
        src, []
    in
    let dst, c2 = 
      if intersect dst group then
        let t = Temp.new_temp () in
        replace dst t, Codegen.codegen_one (T.SMove(F.exp access (T.ETemp F.fp), T.ETemp t))
      else
        dst, []
    in
    let i =
      match i with
      | A.ILabel _ -> i
      | A.IMove r -> A.IMove { r with dst = List.nth dst 0; src = List.nth src 0 }
      | A.IOper r -> A.IOper { r with dst = dst; src = src }
    in
    c1 @ [i] @ c2
  ) code)

let rec alloc (code : A.instr list) (frame : F.frame) : A.instr list * allocation = 
  let cfg = Control_flow.build_control_flow_graph code in
  let liveness_info = Liveness.build_interference_graph cfg in
  let precolored = F.temp_to_reg_mapping in
  let allocation, spilled = Register_coloring.color liveness_info precolored in
  if TempSet.is_empty spilled then
    let code = remove_dummy_moves code allocation in
    let code = X86_frame.proc_entry_exit3 frame code in
    code, allocation
  else
    let spilled = ref spilled in
    let groups = ref [] in
    let code = ref code in
    let rec loop () = 
      if TempSet.is_empty !spilled then
        ()
      else
        let new_group =
          TempSet.fold (fun t acc ->
            let can_add =
              List.for_all (fun t' ->
                Liveness.TempGraph.mem_edge liveness_info.igraph t t'
              ) acc
            in
            if can_add then t :: acc else acc
          ) !spilled []
        in
        code := handle_spill !code frame new_group;
        spilled := TempSet.diff !spilled (TempSet.of_list new_group);
        loop ()
    in
    loop ();
    alloc !code frame
    

