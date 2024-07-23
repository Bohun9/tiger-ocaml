open Frontend
module CF = Control_flow
module ControlFlowGraph = CF.ControlFlowGraph
module NodeHashtbl = CF.NodeHashtbl
module NodeMap = CF.NodeMap
module TempSet = Temp.TempSet

let (+) = TempSet.union
let (-) = TempSet.diff

module TempGraph' = Graph.Imperative.Graph.Concrete(struct
  type t = Temp.temp
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)

module TempGraph = struct
  include TempGraph'

  let get_edges_list g = fold_edges (fun x y acc -> (x, y) :: acc) g []
  let get_nodes_list g = fold_vertex (fun x acc -> x :: acc) g []
  let get_nodes_set g = TempSet.of_list (get_nodes_list g)
  let get_adj_nodes_list g u = fold_succ (fun x acc -> x :: acc) g u []
  let get_adj_nodes_set g u = TempSet.of_list (get_adj_nodes_list g u)
end

type liveness_info = {
  igraph : TempGraph.t;
  mgraph : TempGraph.t
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
      (NodeHashtbl.add h n (state h n + s); true)
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

let build_interference_graph (cfg : CF.cfg) : liveness_info = 
  let entry, exit = liveness cfg in
  let igraph = TempGraph.create ~size:63 () in
  let mgraph = TempGraph.create ~size:63 () in
  let initize g = 
    List.iter (fun t ->
      TempGraph.add_vertex g t
    ) (Temp.created_temps ())
  in
  let add_reg_iedges () = 
    let registers = Temp.TempMap.bindings X86_frame.temp_to_reg_mapping |> List.map fst in
    List.iter (fun r1 ->
      List.iter (fun r2 ->
        if r1 != r2 then
          TempGraph.add_edge igraph r1 r2
        else
          ()
      ) registers
    ) registers
  in
  let add_iedges () = 
    ControlFlowGraph.iter_vertex (fun n ->
      let def = NodeMap.find n cfg.def in
      let use = NodeMap.find n cfg.use in
      List.iter (fun d ->
        let exclude = TempSet.singleton d + if NodeMap.find n cfg.is_move then TempSet.of_list use else TempSet.empty in
        let interfered_temps = state exit n - exclude in
        TempSet.iter (fun t ->
          TempGraph.add_edge igraph d t
        ) interfered_temps
      ) def
    ) cfg.graph
  in
  let add_medges () = 
    ControlFlowGraph.iter_vertex (
      fun n -> 
        if NodeMap.find n cfg.is_move then (
          let d = 
            match NodeMap.find n cfg.def with
            | [d] -> d
            | _ -> failwith "definitions of a move should have exactly one element"
          in
          let s = 
            match NodeMap.find n cfg.use with
            | [s] -> s
            | _ -> failwith "uses of a move should have exactly one element"
          in
          (* s can be equal d in case of a tail function call. *)
          if s != d then
            TempGraph.add_edge mgraph d s
          else
            ()
        ) else 
          ()
    ) cfg.graph
  in
  initize igraph;
  add_reg_iedges ();
  add_iedges ();
  initize mgraph;
  add_medges ();
  {
    igraph = igraph;
    mgraph = mgraph
  }

let show_graph (g : TempGraph.t) (show_temp : Temp.temp -> string) : unit = 
  let vertices = TempGraph.fold_vertex (fun t' acc -> t' :: acc) g [] in
  let vertices = List.sort compare vertices in
  List.iter (fun t ->
    let neighbours = TempGraph.fold_succ (fun t' acc -> t' :: acc) g t [] in
    let neighbours = List.sort compare neighbours in
    let neighbours = List.map show_temp neighbours in
    let neighbours = String.concat "," neighbours in
    print_endline (Printf.sprintf "%-*s [%s]" 4 (show_temp t) neighbours)
  ) vertices

let show_liveness (g : liveness_info) (show_temp : Temp.temp -> string) : unit =
  let _ = print_endline "-----------INTERFERENCE GRAPH------------" in
  show_graph g.igraph show_temp;
  let _ = print_endline "-----------MOVES GRAPH------------" in
  show_graph g.mgraph show_temp
