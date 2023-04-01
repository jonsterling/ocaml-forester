type addr = string

type renderer = Format.formatter -> unit

class type tree = 
  object 
    method process : open_forest -> addr -> unit
    method render : closed_forest -> renderer
  end

and open_forest = 
  object 
    method process : addr -> tree -> unit
    method plant_tree : addr -> tree -> unit
    method record_link : src:addr -> dest:addr -> unit
    method record_translusion : parent:addr -> child:addr -> unit
    method import_macros : at:addr -> dep:addr -> unit
    method def_macro : addr -> name:string -> body:(tree list -> tree) -> unit
    method set_title : addr -> tree -> unit
  end

and closed_forest =
  object 
    method lookup_tree : addr -> tree
    method lookup_backlinks : addr -> addr list
    method lookup_title : addr -> tree option
    method lookup_macro : addr -> name:string -> args:tree list -> tree
    method render_all : renderer
  end

type closure = tree list -> tree

