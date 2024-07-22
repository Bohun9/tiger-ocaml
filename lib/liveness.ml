module CF = Control_flow
module ControlFlowGraph = CF.ControlFlowGraph
module NodeHashtbl = CF.NodeHashtbl
module NodeMap = CF.NodeMap
module TempSet = Temp.TempSet

let (+) = TempSet.union
let (-) = TempSet.diff

module InterferenceGraph = Graph.Imperative.Graph.Concrete(struct
  type t = Temp.temp
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

type interference_graph = {
  graph : InterferenceGraph.t;
  moves : (Temp.temp * Temp.temp) list
}

type live_info = TempSet.t NodeHashtbl.t

let state h n = NodeHashtbl.find h n

let liveness (cfg : CF.cfg) : live_info * live_info = 
  let size = (ControlFlowGraph.nb_vertex cfg.graph) in
  let entry = NodeHashtbl.create size in
  let exit = NodeHashtbl.create size in
  let initize () =
    ControlFlowGraph.iter_vertex (
      fun n -> 
        NodeHashtbl.add entry n TempSet.empty;
        NodeHashtbl.add exit n TempSet.empty
    ) cfg.graph;
  in
  let relax_set h n s = 
    if TempSet.subset s (state h n) then
      false
    else
      (NodeHashtbl.add h n s; true)
  in
  let process_node n = 
    let def = TempSet.of_list (NodeMap.find n cfg.def) in
    let use = TempSet.of_list (NodeMap.find n cfg.use) in
    let b1 = relax_set entry n (use + (state exit n - def)) in
    let b2 = ControlFlowGraph.fold_succ (
        fun n' b -> relax_set exit n (state entry n') || b
      ) cfg.graph n false
    in
    b1 || b2
  in
  let round () =
    ControlFlowGraph.fold_vertex (
      fun n b -> process_node n || b
    ) cfg.graph false
  in
  let rec iteration () = 
    if round () then
      iteration () 
    else
      ()
  in
  initize ();
  iteration ();
  entry, exit

let build_interference_graph (cfg : CF.cfg) : interference_graph = 
  let entry, exit = liveness cfg in
  let graph = InterferenceGraph.create ~size:63 () in
  let initize () = 
    List.iter (fun t ->
      InterferenceGraph.add_vertex graph t
    ) (Temp.created_temps ())
  in
  let add_edges () = 
    ControlFlowGraph.iter_vertex (fun n ->
      let def = NodeMap.find n cfg.def in
      let use = NodeMap.find n cfg.use in
      List.iter (fun d ->
        let exclude = TempSet.singleton d + if NodeMap.find n cfg.is_move then TempSet.of_list use else TempSet.empty in
        let interfered_temps = state exit n - exclude in
        TempSet.iter (fun t ->
          InterferenceGraph.add_edge graph d t
        ) interfered_temps
      ) def
    ) cfg.graph
  in
  let moves = 
    ControlFlowGraph.fold_vertex (
      fun n acc -> 
        if NodeMap.find n cfg.is_move then (
          let d = 
            match NodeMap.find n cfg.def with
            | [d] -> d
            | _ -> failwith "definitions of a move should have exactly one element"
          in
          let s = 
            match NodeMap.find n cfg.def with
            | [s] -> s
            | _ -> failwith "uses of a move should have exactly one element"
          in
          (d, s) :: acc
        ) else 
          acc
    ) cfg.graph []
  in
  initize ();
  add_edges ();
  {
    graph = graph;
    moves = moves
  }

let show_graph (igraph : interference_graph) (show_temp : Temp.temp -> string) : unit = 
  let _ = print_endline "-----------INTERFERENCE GRAPH------------" in
  let vertices = InterferenceGraph.fold_vertex (fun t' acc -> t' :: acc) igraph.graph [] in
  let vertices = List.sort compare vertices in
  List.iter (fun t ->
    let neighbours = InterferenceGraph.fold_succ (fun t' acc -> t' :: acc) igraph.graph t [] in
    let neighbours = List.sort compare neighbours in
    let neighbours = List.map show_temp neighbours in
    let neighbours = String.concat "," neighbours in
    print_endline (Printf.sprintf "%-*s [%s]" 4 (show_temp t) neighbours)
  ) vertices

