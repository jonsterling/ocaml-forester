(* module Xml =
   struct
   type printer = Format.formatter -> unit 

   let rec seq (printers : printer list) : printer = 
    match printers with 
    | [] -> fun _ -> () 
    | u :: vs -> 
      fun fmt ->
        u fmt ; seq vs fmt

   let tag ?(attrs = []) (name : string) body fmt = 
    Format.fprintf fmt "<%s" name;
    begin
      attrs |> List.iter @@ fun (k, v) -> 
      Format.fprintf fmt " %s=\"%s\"" k v
    end;
    Format.fprintf fmt ">";
    body fmt;
    Format.fprintf fmt "</%s>" name
   end

   module Html =
   struct 
   let a ~href =
    Xml.tag "a" ~attrs:["href", href] 

   let li = Xml.tag "li"
   let ul = Xml.tag "ul"
   let nav = Xml.tag "nav"
   end *)
