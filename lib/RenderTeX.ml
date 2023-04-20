open Types 

let rec render_node : Sem.node -> string = 
  function 
  | Sem.Text txt -> String.trim txt
  | Sem.Math x -> render_nodes x 
  | Sem.Tag (name, attrs, args) ->
    "\\" ^ name ^ render_attrs attrs ^ render_args args
  | _ -> failwith "render_nodes"

and render_nodes (nodes : Sem.t) : string = 
  List.fold_right (fun y r -> render_node y ^ r) nodes ""

and render_arg (arg : Sem.t) : string = 
  "{" ^ render_nodes arg ^ "}"

and render_attrs (attrs : Sem.attr list) : string = 
  match List.length attrs with 
  | 0 -> "" 
  | n -> "{" ^ String.concat "," (List.map render_attr attrs) ^ "}"

and render_attr (attr : Sem.attr) : string = 
  let k, v = attr in 
  k ^ " = " ^ v

and render_args (args : Sem.t list) : string = 
  match args with 
  | [] -> "{}"
  | _ -> List.fold_right (fun x r -> render_arg x ^ r) args ""

