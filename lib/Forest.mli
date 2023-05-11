open Types

class forest : 
  size:int ->
  root:addr option -> 
  object 
    method plant_tree : addr -> Expr.doc -> unit
    method render_trees : unit
  end
