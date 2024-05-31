open Dream_html

let reserved_prefix = "fr"
let forester_xmlns = "http://www.jonmsterling.com/jms-005P.xml"

let register_ns tag attrs =
  let f_xmlns = string_attr ("xmlns:" ^ reserved_prefix) "%s" forester_xmlns in
  tag (f_xmlns :: attrs)

let add_ns name = Format.sprintf "%s:%s" reserved_prefix name

let null = HTML.null
let null_ = HTML.null_

let f_std_tag name = std_tag @@ add_ns name
let f_text_tag name = text_tag @@ add_ns name

let f_void_tag name attrs = std_tag (add_ns name) attrs []

let info = f_std_tag "info"

let tree = f_std_tag "tree"
let numbered = bool_attr "numbered"
let toc = bool_attr "toc"
let expanded = bool_attr "expanded"
let show_heading = bool_attr "show-heading"
let show_metadata = bool_attr "show-metadata"
let root = bool_attr "root"

let frontmatter = f_std_tag "frontmatter"
let mainmatter = f_std_tag "mainmatter"
let backmatter = f_std_tag "backmatter"
let contributions = f_std_tag "contributions"
let context = f_std_tag "context"
let related = f_std_tag "related"
let backlinks = f_std_tag "backlinks"
let references = f_std_tag "references"
let anchor attrs = f_text_tag "anchor" attrs
let taxon attrs = f_text_tag "taxon" attrs
let addr attrs = f_text_tag "addr" attrs
let route attrs = f_text_tag "route" attrs
let source_path attrs = f_text_tag "source-path" attrs
let href fmt = uri_attr "href" fmt
let date = f_std_tag "date"
let last_changed = f_std_tag "last-changed"
let year attrs = f_text_tag "year" attrs
let month attrs = f_text_tag "month" attrs
let day attrs = f_text_tag "day" attrs
let authors = f_std_tag "authors"
let author = f_std_tag "author"
let contributor = f_std_tag "contributor"
let title = f_std_tag "title"

let link = f_std_tag "link"
let type_ fmt = string_attr "type" fmt
let addr_ fmt = string_attr "addr" fmt

let number attrs = f_text_tag "number" attrs
let parent attrs = f_text_tag "parent" attrs

let meta = f_std_tag "meta"
let name fmt = string_attr "name" fmt

let tex attrs = f_text_tag "tex" attrs
let display fmt = string_attr "display" fmt

let title_ fmt = string_attr "title" fmt

let prim p =
  let name =
    match p with
    | `P -> "p"
    | `Ul -> "ul"
    | `Ol -> "ol"
    | `Li -> "li"
    | `Em -> "em"
    | `Strong -> "strong"
    | `Code -> "code"
    | `Blockquote -> "blockquote"
    | `Pre -> "pre"
  in
  f_std_tag name

let ref = f_void_tag "ref"

let taxon_ fmt = string_attr "taxon" fmt
let number_ fmt = string_attr "number" fmt

let img = f_void_tag "img"
let src fmt = uri_attr "src" fmt

let embedded_tex = f_std_tag "embedded-tex"
let embedded_tex_preamble attrs = f_text_tag "embedded-tex-preamble" attrs
let embedded_tex_body attrs = f_text_tag "embedded-tex-body" attrs
let hash fmt = string_attr "hash" fmt