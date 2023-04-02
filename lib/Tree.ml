open Types

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

    method render forest fmt = 
      Format.fprintf fmt "<%s>" tag; 
      body#render forest fmt; 
      Format.fprintf fmt "</%s>" tag
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
    method render forest fmt =
      Format.fprintf fmt "<math xmlns=\"http://www.w3.org/1998/Math/MathML\">";
      body#render forest fmt;
      Format.fprintf fmt "</math>"
  end

class wikilink (title : tree) (destAddr : addr) : tree = 
  object 
    method process forest addr = 
      forest#process addr title;
      forest#record_link ~src:addr ~dest:destAddr

    method render forest fmt = 
      Format.fprintf fmt "<a href=\"%s\">" destAddr;
      title#render forest fmt;
      Format.fprintf fmt "</a>"
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

    method render (forest : closed_forest) fmt = 
      let child = forest#lookup_tree childAddr in 
      let backlinks = forest#lookup_backlinks childAddr in 
      child#render forest fmt;

      match backlinks with 
      | [] -> () 
      | _ -> 
        Format.fprintf fmt "<nav>";
        Format.fprintf fmt "<ul>";
        begin 
          backlinks |> List.iter @@ fun addr -> 
          Format.fprintf fmt "<li>";
          Format.fprintf fmt "<a href=\"%s\">" addr;
          begin 
            match forest#lookup_title addr with 
            | None -> Format.fprintf fmt "[%s]" addr
            | Some title -> title#render forest fmt
          end;
          Format.fprintf fmt "</a>";
          Format.fprintf fmt "</li>"
        end; 
        Format.fprintf fmt "</ul>";
        Format.fprintf fmt "</nav>"
  end

