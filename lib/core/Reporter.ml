module Message =
struct
  type t =
    | TreeNotFound
    | DuplicateTree
    | ParseError
    | TypeError
    | ResolverError
    | ExpansionError
    | CreatedTree
    | RanShellCommand
    | FrontmatterInBody
  [@@deriving show]

  let default_severity : t -> Asai.Diagnostic.severity =
    function
    | DuplicateTree -> Error
    | TreeNotFound -> Error
    | ParseError -> Error
    | TypeError -> Error
    | ResolverError -> Error
    | ExpansionError -> Error
    | CreatedTree -> Info
    | RanShellCommand -> Info
    | FrontmatterInBody -> Error

  let short_code : t -> string =
    show
end

include Asai.Reporter.Make (Message)