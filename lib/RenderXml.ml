open Types

type printer = Xmlm.output -> unit
type env = RenderEnv.t

type part = 
  | Top 
  | Frontmatter
  | Mainmatter 
  | Backmatter

type cfg = {part : part}

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

module Xml =
struct
  let tag name attrs bdy : printer =
    let attrs' = attrs |> List.map @@ fun (k,v) -> ("", k), v in
    fun out ->
      Xmlm.output out @@ `El_start (("", name), attrs');
      Printer.seq ~sep:Printer.space bdy out;
      Xmlm.output out `El_end

  let with_xsl stylesheet bdy : printer =
    fun out ->
    let line = Format.sprintf "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" stylesheet in
    Xmlm.output out @@ `Dtd (Some line);
    bdy out
end

let rec render_node ~cfg (env : env) : Sem.node -> printer =
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
    render_internal_link ~cfg env ~title ~addr
  | Sem.Tag (name, attrs, xs) ->
    Xml.tag name attrs
      [xs |> Printer.iter ~sep:Printer.space (render ~cfg env)]
  | Sem.Transclude (mode, addr) ->
    begin
      match env#get_doc addr, mode with 
      | None, _ -> 
        failwith @@ Format.sprintf "Failed to transclude non-existent tree with address '%s'" addr
      | Some doc, mode -> 
        render_doc ~mode ~cfg env doc
    end
  | Sem.EmbedTeX bdy ->
    let code = 
      RenderMathMode.Printer.contents @@ 
      RenderMathMode.render bdy 
    in
    let hash = TeXHash.hash code in
    env#enqueue_svg ~name:hash ~source:code;
    let path = Format.sprintf "resources/%s.svg" hash in
    Xml.tag "center" [] 
      [Xml.tag "img" ["src", path] []]
  | Sem.Group (delim, bdy) ->
    let l, r = 
      match delim with 
      | Braces -> "{","}"
      | Squares -> "[","]"
      | Parens -> "(", ")"
    in 
    Printer.seq
      [Printer.text l;
       render ~cfg env bdy;
       Printer.text r]
  | Sem.Block (title, body) -> 
    Xml.tag "section" ["class", "block"]
      [Xml.tag "details" ["open", ""]
         [Xml.tag "summary" [] [render ~cfg env title];
          render ~cfg env body]]


and render_internal_link ~cfg env ~title ~addr = 
  let url = env#route addr in
  Xml.tag "a" 
    ["href", url; "class", "local"] 
    [render ~cfg env title]

and render ~cfg (env : env) : Sem.t -> printer =
  Printer.iter (render_node ~cfg env)

and render_doc_title ~cfg (env : env) (doc : Sem.doc) = 
  let content =
    match doc.taxon with 
    | Some taxon ->
      Printer.seq ~sep:Printer.space
        [Printer.text @@ StringUtil.title_case taxon;
         match doc.title with 
         | [] -> Printer.nil 
         | _ ->
           Printer.seq 
             [Printer.text "(";
              render ~cfg env doc.title;
              Printer.text ")"]]
    | None ->
      match doc.title with 
      | [] -> 
        Printer.text "Untitled tree"
      | _ -> 
        render ~cfg env @@
        Sem.map_text StringUtil.title_case doc.title
  in
  let url = env#route doc.addr in
  let link =
    Xml.tag "a" ["class", "slug"; "href", url] 
      [Printer.text @@ Format.sprintf "[%s]" doc.addr]
  in
  Xml.tag "h1" [] [content; link]

and render_author (env : env) (author : string) = 
  let cfg = {part = Frontmatter} in
  (* If the author string is an address to a biographical page, then link to it *)
  match env#get_doc author with 
  | Some bio -> 
    let url = env#route bio.addr in
    Xml.tag "a" 
      ["href", url; "class", "local"; "rel", "author"] 
      [render ~cfg env bio.title]
  | None -> 
    Printer.text author

and render_date (doc : Sem.doc) = 
  match doc.date with 
  | None -> Printer.nil 
  | Some date -> 
    let str = Format.asprintf "%a" Date.pp_human date in
    Xml.tag "date" [] [Printer.text str]

and render_authors env (doc : Sem.doc) = 
  match doc.authors, env#get_contributors doc.addr with 
  | [], [] -> Printer.nil
  | authors, contributors ->
    Xml.tag "authors" [] [
      begin 
        authors |> Printer.iter @@ fun author -> 
        Xml.tag "author" [] [render_author env author]
      end;
      begin 
        contributors |> Printer.iter @@ fun contributor -> 
        Xml.tag "contributor" [] [render_author env contributor]
      end
    ]

and render_frontmatter (env : env) (doc : Sem.doc) = 
  Xml.tag "frontmatter" [] [
    Xml.tag "addr" [] [Printer.text doc.addr];
    Xml.tag "route" [] [Printer.text @@ env#route doc.addr];
    render_date doc;
    render_authors env doc;
    Xml.tag "title" [] [
      render ~cfg:{part = Frontmatter} env @@ 
      Sem.map_text StringUtil.title_case doc.title
    ];
    begin 
      match doc.taxon with 
      | None -> Printer.nil 
      | Some taxon -> Xml.tag "taxon" [] [Printer.text @@ StringUtil.title_case taxon]
    end
  ]

and render_mainmatter (env : env) (doc : Sem.doc) = 
  Xml.tag "mainmatter" [] [
    render ~cfg:{part = Mainmatter} env doc.body
  ]

and render_backmatter (env : env) (doc : Sem.doc) = 
  let cfg = {part = Backmatter} in
  Xml.tag "backmatter" [] [
    Xml.tag "contributions" [] [
      env#get_pages_authored doc.addr |> Printer.iter @@ 
      render_doc ~cfg env
    ];
    Xml.tag "context" [] [
      env#get_parents doc.addr |> Printer.iter @@ 
      render_doc ~cfg env
    ];
    Xml.tag "related" [] [
      env#get_links doc.addr |> Printer.iter @@ 
      render_doc ~cfg env
    ];
    Xml.tag "backlinks" [] [
      env#get_backlinks doc.addr |> Printer.iter @@ 
      render_doc ~cfg env
    ]
  ]

and mode_to_string = 
  function 
  | Full -> "full" 
  | Spliced -> "spliced"
  | Collapsed -> "collapsed"

and render_doc ~cfg ?(mode = Full) (env : env) (doc : Sem.doc) : printer =
  Xml.tag "tree" 
    ["mode", mode_to_string mode]
    [render_frontmatter env doc;
     render_mainmatter env doc;
     match cfg.part with 
     | Top -> render_backmatter env doc 
     | _ -> Printer.nil]

let render_doc_page (env : env) (scope : addr) (doc : Sem.doc) : printer =
  Xml.with_xsl "forest.xsl" @@ 
  render_doc ~cfg:{part = Top} env doc

