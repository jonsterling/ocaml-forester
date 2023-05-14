open Core

class forest : 
  size:int ->
  root:addr option -> 
  object 
    method plant_tree : abspath:string option -> addr -> Code.doc -> unit
    method render_trees : unit
  end
