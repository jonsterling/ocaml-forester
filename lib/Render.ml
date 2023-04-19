open Types

type printer = Xmlm.output -> unit 

class type env = 
  object
    method route : addr -> string
    method transclude : addr -> printer
  end

module Printer =
struct 
  let text txt : printer = 
    fun out -> 
      Xmlm.output out @@ `Data txt

  let trimmedText (txt : string) : printer =
    let txt = String.trim txt in 
    if String.length txt > 0 then 
      text @@ txt 
    else 
      fun _ -> ()

  let iter printer xs : printer =
    fun out -> 
    let n = List.length xs in
    xs |> List.iteri @@ fun i x -> 
    if not (i = 0 || i = n) then 
      text " " out;
    printer x out

  let seq ps : printer = 
    iter (fun p -> p) ps

  let nil : printer = fun _ -> ()
end

module Html = 
struct
  let tag name attrs bdy : printer = 
    let attrs' = attrs |> List.map @@ fun (k,v) -> (("", k), v) in 
    fun out ->
      Xmlm.output out (`El_start (("", name), attrs'));
      bdy out; 
      Xmlm.output out `El_end

  let a ~href bdy : printer = 
    tag "a" ["href", href] bdy

  let section bdy : printer = 
    tag "section" [] bdy

  let h1 bdy : printer = 
    tag "h1" [] bdy

  let with_dtd bdy : printer = 
    fun out -> 
    Xmlm.output out @@ `Dtd (Some "<!DOCTYPE html>");
    bdy out

  let html = tag "html" []
  let body = tag "body" []
  let head = tag "head" [] 
  let title = tag "title" [] 
end

let rec renderMathMode (env : env) : Sem.t -> string = 
  function 
  | Sem.Text txt -> String.trim txt
  | Sem.Seq xs -> List.fold_right (fun y r -> renderMathMode env y ^ r) xs ""
  | Sem.Math x -> renderMathMode env x 
  | Sem.Tag (name, [], args) ->
    "\\" ^ name ^ 
    begin 
      match args with 
      | [] -> "{}"
      | _ -> List.fold_right (fun x r -> renderMathArg env x ^ r) args ""
    end
  | _ -> failwith "renderMathMode"

and renderMathArg (env : env) (arg : Sem.t) : string = 
  "{" ^ renderMathMode env arg ^ "}"

let rec render (env : env) : Sem.t -> printer = 
  function 
  | Sem.Text txt -> 
    Printer.trimmedText txt 
  | Sem.Seq xs ->
    xs |> Printer.iter (render env)
  | Sem.Math bdy -> 
    Printer.text @@ "\\(" ^ renderMathMode env bdy ^ "\\)"
  | Sem.Wikilink (title, addr) -> 
    let url = env#route addr in
    Html.a ~href:url @@ render env title
  | Sem.Tag (name, attrs, xs) -> 
    xs |> Printer.iter (render env) |> Html.tag name attrs
  | Sem.Transclude addr -> 
    env#transclude addr

let render_doc (env : env) (doc : Sem.doc) : printer =
  Html.section @@ 
  Printer.seq
    [Html.h1 @@ render env doc.title;
     render env doc.body]

module KaTeX = 
struct 
  open Printer 
  open Html

  let stylesheet : printer = 
    tag "link" 
      ["rel", "stylesheet";
       "href", "https://cdn.jsdelivr.net/npm/katex@0.16.6/dist/katex.min.css";
       "integrity", "sha384-mXD7x5S50Ko38scHSnD4egvoExgMPbrseZorkbE49evAfv9nNcbrXJ8LLNsDgh9d";
       "crossorigin", "anonymous"]
      nil

  let script : printer = 
    tag "script" 
      ["defer", "true";
       "src", "https://cdn.jsdelivr.net/npm/katex@0.16.6/dist/katex.min.js";
       "integrity", "sha384-j/ZricySXBnNMJy9meJCtyXTKMhIJ42heyr7oAdxTDBy/CYA9hzpMo+YTNV5C+1X";
       "crossorigin", "anonymous"] 
    @@ text ""

  let autorender : printer = 
    tag "script"
      ["defer", "true";
       "src", "https://cdn.jsdelivr.net/npm/katex@0.16.6/dist/contrib/auto-render.min.js";
       "integrity", "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05";
       "crossorigin", "anonymous";
       "onload", "renderMathInElement(document.body);"]
    @@ text ""

  let prelude : printer = 
    seq [stylesheet; script; autorender]
end
let render_doc_page (env : env) (doc : Sem.doc) : printer = 
  Html.with_dtd @@ 
  Html.html @@
  Printer.seq
    [Html.head @@ 
     Printer.seq 
       [Html.title @@ render env doc.title;
        KaTeX.prelude];
     Html.body @@ render_doc env doc]
