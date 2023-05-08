open Types

type printer = Xmlm.output -> unit

class type env =
  object
    method route : addr -> string
    method get_title : addr -> Sem.t
    method transclude : addr -> printer
    method enqueue_svg : name:string -> source:string -> unit
  end

module Printer =
struct
  module P0 =
  struct
    type out = Xmlm.output

    let text txt out =
      Xmlm.output out @@ `Data txt
  end

  include PrinterKit.Kit (P0)
end

module Html =
struct
  let tag name attrs bdy : printer =
    let attrs' = attrs |> List.map @@ fun (k,v) -> ("", k), v in
    fun out ->
      Xmlm.output out @@ `El_start (("", name), attrs');
      Printer.seq ~sep:Printer.space bdy out;
      Xmlm.output out `El_end

  let with_dtd bdy : printer =
    fun out ->
    Xmlm.output out @@ `Dtd (Some "<!DOCTYPE html>");
    bdy out
end

let rec render_node (env : env) (scope : addr) : Sem.node -> printer =
  function
  | Sem.Text txt ->
    Printer.text txt
  | Sem.Math (mode, bdy) ->
    let l, r = 
      match mode with 
      | Inline -> "\\(", "\\)"
      | Display -> "\\[", "\\]"
    in 
    let module TP = RenderMathMode.Printer in
    Printer.text @@
    TP.contents @@
    TP.seq
      [TP.text l;
       RenderMathMode.render bdy;
       TP.text r]
  | Sem.Link {title; addr} ->
    let url = env#route addr in
    let title = render env scope title in
    Html.tag "a" ["href", url; "class", "local"] [title]
  | Sem.Tag (name, attrs, xs) ->
    Html.tag name attrs
      [xs |> Printer.iter ~sep:Printer.space (render env scope)]
  | Sem.Transclude addr ->
    env#transclude addr
  | Sem.EmbedTeX bdy ->
    let code = 
      RenderMathMode.Printer.contents @@ 
      RenderMathMode.render bdy 
    in
    let hash = TeXHash.hash code in
    env#enqueue_svg ~name:hash ~source:code;
    let path = Format.sprintf "resources/%s.svg" hash in
    Html.tag "center" [] 
      [Html.tag "img" ["src", path] []]
  | Sem.Group (delim, bdy) ->
    let l, r = 
      match delim with 
      | Braces -> "{","}"
      | Squares -> "[","]"
      | Parens -> "(", ")"
    in 
    Printer.seq
      [Printer.text l;
       render env scope bdy;
       Printer.text r]

and render (env : env) (scope : addr) : Sem.t -> printer =
  Printer.iter (render_node env scope)

let render_doc (env : env) (scope : addr) (doc : Sem.doc) : printer =
  let module TP = RenderMathMode.Printer in
  let heading_content =
    match doc.taxon with 
    | Some taxon ->
      Printer.seq ~sep:Printer.space
        [Printer.text @@ StringUtil.title_case taxon;
         Printer.seq 
           [Printer.text "(";
            render env scope doc.title;
            Printer.text ")"]]
    | None ->
      render env scope @@ 
      Sem.map_text StringUtil.title_case doc.title 
  in
  Html.tag "section" ["class", "block"]
    [Html.tag "details" ["open","true"]
       [Html.tag "summary" []
          [Html.tag "header" []
             [Html.tag "h1" [] [heading_content]]];
        Html.tag "div" ["class", "post-content"]
          [render env scope doc.body]]]

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
      []

  let script : printer =
    tag "script"
      ["defer", "true";
       "src", "https://cdn.jsdelivr.net/npm/katex@0.16.6/dist/katex.min.js";
       "integrity", "sha384-j/ZricySXBnNMJy9meJCtyXTKMhIJ42heyr7oAdxTDBy/CYA9hzpMo+YTNV5C+1X";
       "crossorigin", "anonymous"]
      [text ""]

  let autorender : printer =
    tag "script"
      ["defer", "true";
       "src", "https://cdn.jsdelivr.net/npm/katex@0.16.6/dist/contrib/auto-render.min.js";
       "integrity", "sha384-+VBxd3r6XgURycqtZ117nYw44OOcIax56Z4dCRWbxyPt0Koah1uHoK0o4+/RRE05";
       "crossorigin", "anonymous";
       "onload", "renderMathInElement(document.body);"]
      [text ""]

  let prelude : printer =
    seq [stylesheet; script; autorender]
end

let render_doc_page (env : env) (scope : addr) (doc : Sem.doc) : printer =
  Html.with_dtd @@
  Html.tag "html" []
    [Html.tag "head" []
       [Html.tag "title" [] [render env scope doc.title];
        Html.tag "link"
          ["rel", "stylesheet";
           "href", "style.css"]
          [];
        Html.tag "link"
          ["rel", "stylesheet";
           "href", "https://fonts.googleapis.com/css2?family=Inria+Sans:ital,wght@0,300;0,400;0,700;1,300;1,400;1,700&amp;display=swap"]
          [];
        KaTeX.prelude];
     Html.tag "body" [] [render_doc env scope doc]]
