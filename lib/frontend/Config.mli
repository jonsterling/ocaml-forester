module Forest_config : sig
  type t =
    {trees : string list;
     assets : string list;
     theme : string;
     base_url : string option;
     root : string option;
     stylesheet : string}
end

val default_forest_config : Forest_config.t
val parse_forest_config_file : string -> Forest_config.t
