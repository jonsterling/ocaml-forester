open Types

module Xml =
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
end

class empty : tree = 
  object 
    method process _ _ = () 
    method render _ _ = () 
  end

class plain_text (body : string) : tree = 
  object
    method process _ _ = ()
    method render _ fmt = 
      Format.fprintf fmt "%s" body
  end

class basic_tag (tag : string) (body : tree) : tree = 
  object 
    method process forest addr = 
      forest#process addr body

    method render forest = 
      Xml.tag tag @@
      body#render forest 
  end

class bold : tree -> tree = 
  basic_tag "b"

class emph : tree -> tree = 
  basic_tag "em"

class paragraph : tree -> tree = 
  basic_tag "p"

class import_macros (dep : addr) : tree = 
  object 
    method process forest addr = 
      forest#import_macros ~at:addr ~dep

    method render _ _ = ()
  end

class set_title (title : tree) : tree =
  object 
    method process forest addr = 
      forest#process addr title;
      forest#set_title addr title

    method render _ _ = ()
  end

class def_macro ~(name : string) (body : tree list -> tree) : tree = 
  object
    method process forest addr = 
      forest#def_macro addr ~name ~body

    method render _ _ = () 
  end

class use_macro ~(name : string) ~(args : tree list) : tree = 
  object
    val mutable use_site : addr option = None

    method process forest addr : unit = 
      use_site <- Some addr;
      args |> List.iter @@ forest#process addr

    method render forest fmt = 
      let addr : addr = Option.get use_site in 
      let body : tree = forest#lookup_macro addr ~name ~args in
      body#render forest fmt
  end

class glue (trees : tree list) : tree =
  object 
    method process forest addr = 
      List.iter (forest#process addr) trees

    method render forest fmt =
      let task tree = tree#render forest fmt in 
      List.iter task trees
  end

class math (body : tree) : tree = 
  object 
    method process _ _ = () 
    method render forest =
      Xml.tag "math" ~attrs:["xmlns", "http://www.w3.org/1998/Math/MathML"] @@
      body#render forest
  end

class wikilink (title : tree) (destAddr : addr) : tree = 
  object 
    method process forest addr = 
      forest#process addr title;
      forest#record_link ~src:addr ~dest:destAddr

    method render forest = 
      Html.a ~href:destAddr @@
      title#render forest
  end

class transclude (childAddr : addr) : tree = 
  object
    method process forest addr =
      forest#record_translusion ~parent:addr ~child:childAddr

    method render forest = 
      let child = forest#lookup_tree childAddr in
      child#render forest
  end

class title_meta (title : tree) : tree = 
  object 
    method process forest addr = 
      forest#process addr title;
      forest#set_title addr title

    method render =
      title#render
  end

class root (childAddr : addr) : tree = 
  object 
    method process _ _ = ()

    method render (forest : closed_forest) = 
      let child = forest#lookup_tree childAddr in 
      let backlinks = forest#lookup_backlinks childAddr in 
      Xml.seq @@ 
      [ child#render forest;
        match backlinks with 
        | [] -> Xml.seq []
        | _ -> 
          Html.nav @@ 
          Html.ul @@
          Xml.seq @@ 
          begin
            backlinks |> List.map @@ fun addr -> 
            Html.li @@ 
            Html.a ~href:addr @@ 
            match forest#lookup_title addr with 
            | None -> fun fmt -> Format.fprintf fmt "[%s]" addr
            | Some title -> title#render forest
          end
      ]
  end
