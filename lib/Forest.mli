open Types

class forest : 
  size:int ->
  object 
    method plant_tree : addr -> Expr.doc -> unit
    method render_trees : unit
  end
