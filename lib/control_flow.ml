module A = Assem

module ControlFlowGraph = Graph.Imperative.Digraph.Abstract(struct
  type t = A.instr
end)

module NodeMap = Map.Make(struct
  type t = ControlFlowGraph.V.t
  let compare = ControlFlowGraph.V.compare
end)

module NodeHashtbl = Hashtbl.Make(struct
  include ControlFlowGraph.V
end)

type cfg = {
  graph : ControlFlowGraph.t;
  def : Temp.temp list NodeMap.t;
  use : Temp.temp list NodeMap.t;
  is_move : bool NodeMap.t
}

let build_control_flow_graph (code : A.instr list) : cfg = 
  let graph = ControlFlowGraph.create ~size:(List.length code) () in
  let def, use, is_move, code =
    List.fold_right (fun i (d, u, m, c) -> 
      let n = ControlFlowGraph.V.create i in
      let def, use, is_move =
        match i with
        | A.IOper { dst; src; _ } -> dst, src, false
        | A.ILabel _ -> [], [], false
        | A.IMove { dst; src; _ } -> [dst], [src], true
      in
      (NodeMap.add n def d, NodeMap.add n use u, NodeMap.add n is_move m, (i, n) :: c)
    ) code (NodeMap.empty, NodeMap.empty, NodeMap.empty, [])
  in
  let rec add_edges xs = 
    begin match xs with
    | (A.IOper { jump = None; _ }, n1) :: (_, n2) :: _ | (A.ILabel _, n1) :: (_, n2) :: _ | (A.IMove _, n1) :: (_, n2) :: _ ->
        ControlFlowGraph.add_edge graph n1 n2
    | _ -> ()
    end;
    begin match xs with
    | (A.IOper { jump = Some labels; _ }, n1) :: _ ->
        List.iter (fun l -> 
          let (_, n2) = List.find (fun (i, _) -> 
              match i with
              | A.ILabel { lab = l'; _ } -> l = l'
              | _ -> false
            ) code
          in
          ControlFlowGraph.add_edge graph n1 n2
        ) labels
    | _ -> ()
    end;
    begin match xs with
    | [] -> ()
    | _ :: xs -> add_edges xs
    end
  in
  add_edges code;
  {
    graph = graph;
    def = def;
    use = use;
    is_move = is_move
  }
