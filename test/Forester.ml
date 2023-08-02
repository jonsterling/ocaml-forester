module G = Graph.Imperative.Digraph.Concrete (String)
module Topo = Graph.Topological.Make (G)

let gph = G.create ()

let _ =
  G.add_vertex gph "Hello";
  G.add_vertex gph "World";
  G.add_vertex gph "Foo";
  G.add_edge gph "World" "Foo";
  G.add_edge gph "World" "asdfasdf";
  gph |> Topo.iter @@ fun x ->
  Format.printf "Visited %s@." x
