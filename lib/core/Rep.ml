open Repr

(** Reps of Asai types *)

let string_source : Range.string_source t =
  let open Range in
  record "string_source" (fun title content -> { title; content })
  |+ field "title" (option string) (fun s -> s.title)
  |+ field "content" string (fun s -> s.content)
  |> sealr

let source_repr : Range.source ty =
  let open Range in
  variant "source" (fun file string -> function
      | `File s -> file s | `String s -> string s)
  |~ case1 "File" string (fun s -> `File s)
  |~ case1 "String" string_source (fun s -> `String s)
  |> sealv

let position : Range.position ty =
  let open Range in
  record "position" (fun source offset start_of_line line_num ->
      { source; offset; start_of_line; line_num })
  |+ field "source" source_repr (fun t -> t.source)
  |+ field "offset" int (fun t -> t.offset)
  |+ field "start_of_line" int (fun t -> t.offset)
  |+ field "line_num" int (fun t -> t.offset)
  |> sealr

let range : Range.t ty =

  (*  NOTE:
      For the irmin-git backend, the functions we need are pp, of_string and
      equal. I've worked around the need for a full of_string implementation
      (parser), since in `located_sem_node` I am simply returning `None` for the
      value of `loc`. This means even though we can serialize ranges, we can't
      retrieve them. This is fine for now, since we don't need that info for
      rendering, which is our primary use case.
  *)

  let open Range in
  let pp = Range.dump in
  let pos =
    { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
  in

  let of_string str =
    (*  HACK: Should parse this kind of string (produced by Range.dump):

        Range
        ({source=(`File "todo"); offset=0; start_of_line=0; line_num=0},
         {source=(`File "todo"); offset=0; start_of_line=0; line_num=0})
    *)
    Ok (Range.make (pos, pos))
  in

  let r = (Range.make (pos, pos)) in
  let encode encoder range = () in
  let decode _ = Ok r in
  let encode_bin : _ encode_bin = fun _ _ -> () in
  let decode_bin _ _ = r in
  let size_of : _ size_of =
    (* NOTE: Named args of_value and of_encoding are optional.
       Precompute the size that will be used by `encode_bin`. `of_encoding`
       unused nowadays
    *)
    Size.custom_dynamic () in

  let compare_pos p q =
    p.source = q.source &&
    p.offset = q.offset &&
    p.start_of_line = q.start_of_line &&
    p.line_num = q.line_num
  in

  let equal r1 r2 =
    match Range.view r1, Range.view r2 with
    | `End_of_file p, `End_of_file q -> compare_pos p q
    | `Range (p1, p2), `Range (q1, q2) ->
      compare_pos p1 q1 && compare_pos p2 q2
    | _ -> false
  in
  let compare r1 r2 =
    if equal r1 r2 then 0 else
      (*  FIXME: Is this used by the git-backend? If not, remove it.
      *)
      match Range.view r1, Range.view r2 with
      | `End_of_file p, `End_of_file q ->
        (if p.source = q.source then match p.source, q.source with
            | `String s1, `String s2 -> String.compare s1.content s2.content
            | `File s1, `File s2 -> String.compare s1 s2
            | _ -> -1
         else -1)
      | `Range (p1, p2), `Range (q1, q2) -> -1
      | _ -> -1
  in
  let short_hash ?seed a = 0 in
  let pre_hash _ _ = () in
  abstract ~pp ~of_string ~json:(encode, decode)
    ~bin:(encode_bin, decode_bin, size_of)
    ~equal ~compare ~short_hash ~pre_hash ()


let prim : Prim.t ty=
  let open Prim in
  enum "prim"
    [
      ("P", `P);
      ("Ol", `Ol);
      ("Ul", `Ul);
      ("Li", `Li);
      ("Em", `Em);
      ("Strong", `Strong);
      ("Code", `Code);
      ("Blockquote", `Blockquote);
      ("Pre", `Pre);
    ]

let date : Prelude.Date.t ty =
  let open Prelude.Date in
  record "date" (fun yyyy mm dd -> {yyyy; mm; dd})
  |+ field "yyyy" int (fun t -> t.yyyy)
  |+ field "mm" (option int) (fun t -> t.mm)
  |+ field "dd" (option int) (fun t -> t.dd)
  |> sealr

module Tree : Irmin.Contents.S with type t = Sem.tree = struct
  type t = Sem.tree

  let math_mode : Base.math_mode ty =
    let open Base in
    enum "math_mode" [ ("inline", Inline); ("display", Display) ]

  let tex_cs : TeX_cs.t ty =
    variant "tex_cs"
      (fun word symbol ->
         function
         | TeX_cs.Word x -> word x
         | TeX_cs.Symbol x -> symbol x)
    |~ case1 "Word" string (fun x -> TeX_cs.Word x)
    |~ case1 "Symbol" char (fun x -> TeX_cs.Symbol x)
    |> sealv

  let addr : Base.Addr.t ty =
    let open Base in
    variant "addr"
      (fun
        user_addr
        machine_addr
        -> function
          | User_addr x -> user_addr x
          | Machine_addr x -> machine_addr x)
    |~ case1 "User_addr" string (fun x -> User_addr x)
    |~ case1 "Machine_addr" int (fun x -> Machine_addr x)
    |> sealv

  let xml_resolved_qname : Base.xml_resolved_qname ty =
    let open Base in
    record "xml_resolved_qname"
      (fun prefix uname xmlns -> {prefix; uname; xmlns})
    |+ field "prefix" (option string) (fun r -> r.prefix)
    |+ field "uname" string (fun r -> r.uname)
    |+ field "xmlns" (option string) (fun r -> r.xmlns)
    |> sealr

  let rec sem_node (t : Sem.t ty) (tree : Sem.tree ty) : Sem.node ty =
    let open Sem in
    variant "node"
      (fun
        text
        verbatim
        transclude
        subtree
        query
        link
        xml_tag
        tex_cs
        math
        embed_tex
        img
        if_tex
        prim
        object_
        ref
        -> function
          | Text s -> text s
          | Verbatim s -> verbatim s
          | Transclude (x, y) -> transclude (x, y)
          | Subtree (x, y) -> subtree (x, y)
          | Query (x, y) -> query (x, y)
          | Link (x, y, z) -> link (x, y, z)
          | Xml_tag (x, y, z) -> xml_tag (x, y, z)
          | TeX_cs x -> tex_cs x
          | Math (x, y) -> math (x, y)
          | Embed_tex x -> embed_tex x
          | Img x -> img x
          | If_tex (x, y) -> if_tex (x, y)
          | Prim (x, y) -> prim (x, y)
          | Object x -> object_ x
          | Ref x -> ref x)
    |~ case1 "Text" string (fun s -> Text s)
    |~ case1 "Verbatim" string (fun s -> Verbatim s)
    |~ case1 "Transclude"
      (pair (tranclusion_opts t) addr)
      (fun (x, y) -> Transclude (x, y))
    |~ case1 "Subtree"
      (pair (tranclusion_opts t) tree)
      (fun (x, y) -> Subtree (x, y))
    |~ case1 "Query"
      (pair (tranclusion_opts t) (query tree t t))
      (fun (x, y) -> Query (x, y))
    |~ case1 "Link" (triple addr (option t) modifier) (fun (x, y, z) -> Link (x, y, z))
    |~ case1 "Xml_tag"
      (triple xml_resolved_qname (list @@ pair xml_resolved_qname t) t)
      (fun (x, y, z) -> Xml_tag (x, y, z))
    |~ case1 "TeX_cs" tex_cs (fun s -> TeX_cs s)
    |~ case1 "Math" (pair math_mode t) (fun (x, y) -> Math (x, y))
    |~ case1 "Embed_tex" (embed_tex t) (fun s -> Embed_tex s)
    |~ case1 "Img" string (fun s -> Img s)
    |~ case1 "If_tex" (pair t t) (fun (x, y) -> If_tex (x, y))
    |~ case1 "Prim" (pair prim t) (fun (x, y) -> Prim (x, y))
    |~ case1 "Object_" symbol (fun s -> Object s)
    |~ case1 "Ref" addr (fun s -> Ref s)
    |> sealv

  and embed_tex (t : Sem.t ty) : Sem.embedded_tex ty =
    let open Sem in
    record "embed_tex" (fun preamble source -> { preamble; source })
    |+ field "preamble" t (fun t -> t.preamble)
    |+ field "source" t (fun t -> t.source)
    |> sealr

  and modifier : Sem.Text_modifier.t ty =
    let open Sem.Text_modifier in
    enum "modifier" [ ("sentence_case", Sentence_case); ("identity", Identity)]

  and symbol : Symbol.t ty =
    let open Symbol in
    pair (list string) int

  and query (tree : Sem.tree ty) (t : Sem.t ty) a : 'a Query.t ty =
    let open Query in
    mu @@ fun query ->
    variant "query" (fun author tag taxon meta or_ and_ not_ true_ -> function
        | Author x -> author x
        | Tag x -> tag x
        | Taxon x -> taxon x
        | Meta (x, y) -> meta (x, y)
        | Or x -> or_ x
        | And x -> and_ x
        | Not x -> not_ x
        | True -> true_)
    |~ case1 "Author" a (fun x -> Author x)
    |~ case1 "Tag" a (fun x -> Tag x)
    |~ case1 "Taxon" a (fun x -> Taxon x)
    |~ case1 "Meta" (pair string a) (fun (x, y) -> Meta (x, y))
    |~ case1 "Or" (list query ) (fun x -> Or x)
    |~ case1 "And" (list query ) (fun x -> And x)
    |~ case1 "Not" query (fun x -> Not x)
    |~ case0 "True" True |> sealv


  and tranclusion_opts (t : Sem.t ty) =
    let open Sem in
    record "tranclusion_opts"
      (fun
        toc
        show_heading
        show_metadata
        title_override
        taxon_override
        expanded
        numbered
        ->
          {
            toc;
            show_heading;
            show_metadata;
            title_override;
            taxon_override;
            expanded;
            numbered;
          })
    |+ field "toc" bool (fun t -> t.toc)
    |+ field "show_heading" bool (fun t -> t.show_heading)
    |+ field "show_metadata" bool (fun t -> t.show_metadata)
    |+ field "title_override"
      (option t)
      (fun t -> t.title_override)
    |+ field "taxon_override" (option string) (fun t -> t.taxon_override)
    |+ field "expanded" bool (fun t -> t.expanded)
    |+ field "numbered" bool (fun t -> t.numbered)
    |> sealr

  let frontmatter (t : Sem.t ty) =
    let open Sem in
    record "frontmatter"
      (fun
        title
        taxon
        authors
        contributors
        dates
        addr
        metas
        tags
        physical_parent
        designated_parent
        source_path
        number
        ->
          {
            title;
            taxon;
            authors;
            contributors;
            dates;
            addr;
            metas;
            tags;
            physical_parent;
            designated_parent;
            source_path;
            number;
          })
    |+ field "title" (option t) (fun t -> t.title)
    |+ field "taxon" (option string) (fun t -> t.taxon)
    |+ field "authors" (list addr) (fun t -> t.authors)
    |+ field "contributors" (list addr) (fun t -> t.contributors)
    |+ field "dates" (list date) (fun t -> t.dates)
    |+ field "addr" addr (fun t -> t.addr)
    |+ field "metas" (list (pair string t)) (fun t -> t.metas)
    |+ field "tags" (list string) (fun t -> t.tags)
    |+ field "physical_parent" (option addr) (fun t -> t.physical_parent)
    |+ field "designated_parent" (option addr) (fun t -> t.designated_parent)
    |+ field "source_path" (option string) (fun t -> t.source_path)
    |+ field "number" (option string) (fun t -> t.number)
    |> sealr

  let located_sem_node (t : Sem.t ty) (tree : Sem.tree ty) : Sem.node Range.located ty =
    let open Asai in
    let open Range in
    record "located_sem_node" (fun loc value -> { loc; value })
    |+ field "loc" (option range) (fun t -> None)
    |+ field "value" (sem_node t tree) (fun t -> t.value)
    |> sealr

  let tree  : Sem.tree ty =
    let open Sem in
    mu (fun tree ->
        let t = mu (fun t -> list (located_sem_node t tree)) in
        record "tree" (fun fm body : Sem.tree -> { fm; body })
        |+ field "fm" (frontmatter t) (fun t -> t.fm)
        |+ field "body" t (fun (t : Sem.tree) -> t.body)
        |> sealr)

  let t = tree
  let merge = Irmin.Merge.(option (idempotent t))
end
