module Message =
struct
  type t =
    | Tree_not_found
    | Duplicate_tree
    | Parse_error
    | Type_error
    | Resolution_error
    | Expansion_error
    | Duplicate_attribute
    | Frontmatter_in_body
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Profiling
  [@@deriving show]

  let default_severity : t -> Asai.Diagnostic.severity =
    function
    | Duplicate_tree -> Error
    | Tree_not_found -> Error
    | Parse_error -> Error
    | Type_error -> Error
    | Resolution_error -> Error
    | Expansion_error -> Error
    | Duplicate_attribute -> Error
    | Frontmatter_in_body -> Error
    | Unhandled_case -> Bug
    | Transclusion_loop -> Error
    | Internal_error -> Bug
    | Configuration_error -> Error
    | Initialization_warning -> Warning
    | Profiling -> Info

  let short_code : t -> string =
    show
end

include Asai.Reporter.Make (Message)
