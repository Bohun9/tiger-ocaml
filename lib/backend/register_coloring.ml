(* 
   Input:
   1. An interference graph of [temps]
   2. A moves graph of [temps]
   3. A list of precolored temps from the [Frame] module

   Output:
   1. A mapping from [temps] to [Frame.registers] (colors)
   2. A list of spilled [temps]

   Algorithm ideas:
   1. The main phases will iterate through all live vertices, but we will
      maintain both graphs so that the [degree] functions take constant time.

   2. The main invariant throughout procedure is that there are no common edges between  
      the [interference] and [moves] graphs.

   3. When deleting a node from the [interference] graph, we need to remember
      its edges for the color assignment phase.
 *)

open Frontend
module A = Assem
module F = X86_frame
module T = Temp
module G = Liveness.TempGraph
module TempMap = T.TempMap
module TempSet = T.TempSet
module RegSet = F.RegSet

type allocation = F.register TempMap.t

let color ({ igraph; mgraph } : Liveness.liveness_info) (precolored : allocation) : allocation * TempSet.t =
  let precolored_temps = TempMap.bindings precolored |> List.map fst |> TempSet.of_list in
  let k = TempSet.cardinal precolored_temps in
  let select_stack = ref [] in
  let graph_size = G.nb_vertex igraph in
  let saved_neighbours = Hashtbl.create graph_size in
  let alias = Hashtbl.create graph_size in
  let coalesced = ref TempSet.empty in

  let (++) = TempSet.union in
  let (--) = TempSet.diff in

  let is_precolored x = TempSet.mem x precolored_temps in
  let iedge = G.mem_edge igraph in
  let medge = G.mem_edge mgraph in
  let nodes = G.get_nodes_set in
  let degree = G.out_degree in
  let idegree = degree igraph in
  let low_degree u = degree igraph u < k in
  let move_related u = degree mgraph u > 0 in
  let iadj = G.get_adj_nodes_set igraph in
  let madj = G.get_adj_nodes_set mgraph in
  let add_iedge x y = assert (x != y); G.add_edge igraph x y in
  let add_medge x y = assert (x != y); G.add_edge mgraph x y in

  let force_invariant_node u = 
    TempSet.iter (fun v ->
      if iedge u v then
        G.remove_edge mgraph u v
      else
        ()
    ) (madj u)
  in
  let force_invariant () = TempSet.iter force_invariant_node (nodes igraph) in

  let add_to_select_stack u =
    Hashtbl.add saved_neighbours u (iadj u);
    select_stack := u :: !select_stack
  in

  let remove u =
    G.remove_vertex igraph u;
    G.remove_vertex mgraph u
  in

  let simplify () : bool = 
    let res = ref false in
    begin TempSet.iter (fun u ->
      if low_degree u && not(move_related u) then (
        res := true;
        add_to_select_stack u;
        remove u
      ) else
        ()
    ) (nodes igraph -- precolored_temps)
    end;
    !res
  in

  let george x y = 
    let _ = assert (is_precolored y) in
    TempSet.fold (fun z acc ->
      acc && (low_degree z || iedge y z)
    ) (iadj x) true
  in

  let briggs x y = 
    let _ = assert (not (iedge x y)) in
    let zs = iadj x ++ iadj y in
    let significant_num =
      TempSet.fold (fun u acc ->
        let d = idegree u - (if iedge u x && iedge u y then 1 else 0) in
        acc + (if d >= k then 1 else 0)
      ) zs 0
    in
    significant_num < k
  in

  (* x -> y *)
  let can_coalesce x y = 
    let x, y = 
      if is_precolored x then (
        y, x
      ) else
        x, y
    in
    if is_precolored y then
      george x y 
    else
      briggs x y
  in

  let merge x y = 
    let x, y = 
      if is_precolored x then (
        assert (not (is_precolored y));
        y, x
      ) else
        x, y
    in
    coalesced := TempSet.add x !coalesced;
    Hashtbl.add alias x y;
    let is = iadj x in
    let ms = madj x -- TempSet.singleton y in
    TempSet.iter (fun z -> add_iedge y z) is;
    TempSet.iter (fun z -> add_medge y z) ms;
    force_invariant_node y;
    remove x
  in

  let coalesce () : bool =
    let res = ref false in
    begin List.iter (fun (x, y) ->
      if medge x y && can_coalesce x y then (
        assert (not (iedge x y));
        res := true;
        merge x y
      ) else
        ()
    ) (G.get_edges_list mgraph)
    end;
    !res
  in

  let freeze () : bool = 
    let rec aux nodes =
      match nodes with
      | [] -> false
      | x :: xs ->
          if low_degree x && move_related x then (
            G.remove_vertex mgraph x;
            G.add_vertex mgraph x;
            true
          ) else
            aux xs
    in
    aux (G.get_nodes_list igraph)
  in

  let spill () : bool =
    let candidates = nodes igraph -- precolored_temps in
    let c = TempSet.min_elt candidates in
    add_to_select_stack c;
    remove c;
    true
  in

  let rec get_alias x = 
    if TempSet.mem x !coalesced then
      get_alias (Hashtbl.find alias x)
    else
      x
  in

  let assign_colors () : allocation * TempSet.t =
    let color = ref precolored in
    let spilled = ref TempSet.empty in
    let registers = TempMap.bindings precolored |> List.map snd |> RegSet.of_list in
    begin List.iter (fun x -> 
      let available =
        TempSet.fold (fun y acc ->
          let y = get_alias y in
          if TempSet.mem y !spilled then
            acc
          else
            RegSet.remove (TempMap.find y !color) acc
        ) (Hashtbl.find saved_neighbours x) registers
      in
      if RegSet.is_empty available then
        spilled := TempSet.add x !spilled
      else (
        let c = RegSet.min_elt available in
        color := TempMap.add x c !color
      )
    ) !select_stack
    end;
    begin TempSet.iter (fun x -> 
      let y = Hashtbl.find alias x in
      let c = TempMap.find y !color in
      color := TempMap.add x c !color
    ) !coalesced
    end;
    !color, !spilled
  in

  let rec loop () = 
    match G.nb_vertex igraph with
    | n when n < k -> failwith "the number of vertices is less than K"
    | n when n = k -> ()
    | _ ->
        if      simplify () then loop ()
        else if coalesce () then loop ()
        else if freeze ()   then loop ()
        else if spill ()    then loop ()
        else failwith "the spill function should always return true"
  in
  force_invariant ();
  loop ();
  assign_colors ()
