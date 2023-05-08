open Types

module Printer =
struct
  module P0 = 
  struct 
    type out = Format.formatter 
    let text txt fmt =  
      Format.fprintf fmt "%s" txt
  end

  include PrinterKit.Kit (P0)

  let contents (printer : t) : string = 
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()  
end

let rec render_node : Sem.node -> Printer.t =
  function 
  | Sem.Text txt -> 
    Printer.text txt
  | Sem.InlineMath xs | Sem.DisplayMath xs -> 
    render_nodes xs
  | Sem.Tag (name, attrs, args) -> 
    render_tag name attrs args
  | Sem.Group (delim, xs) ->
    render_arg delim xs
  | Sem.EmbedTeX x -> 
    render_nodes x
  | node -> 
    Format.eprintf "missing case: %a@." Sem.pp_node node;
    failwith "RenderTeX.render_node"

and render_tag name attrs args = 
  Printer.seq 
    [Printer.text "\\";
     Printer.text name;
     render_attrs attrs;
     render_args args]

and render_nodes xs =
  Printer.iter ~sep:Printer.space render_node xs

and render_attrs (attrs : Sem.attr list) : Printer.t = 
  match List.length attrs with 
  | 0 -> Printer.nil
  | n -> 
    Printer.seq 
      [Printer.text "{";
       Printer.iter ~sep:(Printer.text ",") render_attr attrs;
       Printer.text "}"]

and render_attr (attr : Sem.attr) : Printer.t  = 
  let k, v = attr in 
  Printer.seq 
    [Printer.text k;
     Printer.text " = "; 
     Printer.text v]

and render_arg delim (arg : Sem.t) : Printer.t =
  let l, r = 
    match delim with 
    | Braces -> "{", "}"
    | Squares -> "[", "]"
    | Parens -> "(", ")"
  in
  Printer.seq 
    [Printer.text l;
     render_nodes arg;
     Printer.text r]

and render_args (args : Sem.t list) : Printer.t = 
  Printer.iter (render_arg Braces) args
