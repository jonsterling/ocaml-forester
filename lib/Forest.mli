open Types

class forest : 
  object 
    method plant_tree : addr -> Expr.doc -> unit
    method render_trees : unit
  end
