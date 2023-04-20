open Types 

module Printer =
struct
  include PrinterKit.Kit (struct type out = Format.formatter end)

  let text txt fmt =  
    Format.fprintf fmt "%s" txt

  let trimmedText (txt : string) : t =
    let txt = String.trim txt in 
    if String.length txt > 0 then 
      text @@ txt 
    else 
      fun _ -> ()

  let contents (printer : t) : string = 
    Format.asprintf "%a" (fun fmt _ -> printer fmt) ()  
end

let rec render_node : Sem.node -> Printer.t =
  function 
  | Sem.Text txt -> 
    Printer.trimmedText txt
  | Sem.Math xs -> 
    render_nodes xs
  | Sem.Tag (name, attrs, args) -> 
    Printer.seq 
      [Printer.text "\\";
       Printer.text name;
       render_attrs attrs;
       render_args args]
  | _ -> 
    failwith "RenderTeX.render_node"

and render_nodes xs =
  Printer.iter render_node xs

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
  Printer.seq [Printer.text k; Printer.text " = "; Printer.text v]

and render_arg (arg : Sem.t) : Printer.t = 
  Printer.seq 
    [Printer.text "{";
     render_nodes arg;
     Printer.text "}"]

and render_args (args : Sem.t list) : Printer.t = 
  match args with 
  | [] -> Printer.text "{}"
  | _ -> Printer.iter render_arg args
