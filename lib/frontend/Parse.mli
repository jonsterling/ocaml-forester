open Core

val parse_file : Eio.Fs.dir_ty Eio.Path.t -> (Code.t, Code.t * Core.Reporter.Message.t Asai.Diagnostic.t list) result
val parse_string : string -> (Code.t, Code.t * Core.Reporter.Message.t Asai.Diagnostic.t list) result
