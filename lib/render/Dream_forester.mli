open Dream_html_lite
open Core

val reserved_prefix : string
val forester_xmlns : string

val register_ns : std_tag -> std_tag

val null : node list -> node
val null_ : attr

val tree : std_tag
val numbered : bool to_attr
val toc : bool to_attr
val expanded : bool to_attr
val show_heading : bool to_attr
val show_metadata : bool to_attr
val root : bool to_attr

val info : std_tag

val frontmatter : std_tag
val mainmatter : std_tag
val backmatter : std_tag
val contributions : std_tag
val context : std_tag
val related : std_tag
val backlinks : std_tag
val references : std_tag

val anchor : _ text_tag
val taxon : _ text_tag
val addr : _ text_tag
val route : _ text_tag
val source_path : _ text_tag
val date : std_tag
val last_changed : std_tag
val title : std_tag

val href : _ string_attr
val year : _ text_tag
val month : _ text_tag
val day : _ text_tag

val authors : std_tag
val author : std_tag
val contributor : std_tag

val link : std_tag
val type_ : _ string_attr
val addr_ : _ string_attr
val title_ : _ string_attr

val number : _ text_tag
val parent : _ text_tag

val meta : std_tag
val name : _ string_attr

val tex : _ text_tag
val display : _ string_attr

val ref : void_tag
val taxon_ : _ string_attr
val number_ : _ string_attr

val img : void_tag
val src : _ string_attr

val prim : Prim.t -> std_tag

val embedded_tex : std_tag
val embedded_tex_preamble : _ text_tag
val embedded_tex_body : _ text_tag
val hash : _ string_attr
