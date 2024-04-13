open Forester.Parse
open Core

let emit _ = () (* ignore *)

let fatal _ = exit 1

let _ =
  Core.Reporter.run ~emit ~fatal @@ fun () ->
  let good = Result.get_ok @@ parse_string {|
    \title{Good}
    \taxon{Test}
    \author{Testy}

    \p{
      This should parse correctly.
    }
    |}
  in
  Printf.printf "parse_good_result:\n%s\n\n" (Code.show good)

let _ =
  Core.Reporter.run ~emit ~fatal @@ fun () ->
  let bad, errors = Result.get_error @@ parse_string {|
    \title{Error recovery}
    \taxon{Test}
    \author{Testy}

    \p{
      This has some syntax errors, but the parser should recover as much as possible.
    }

    Invalid naked text here.

    \p{
      More good text.
    }

    \p{
      Some bad text [
    }

    \p{
      Even more good text.
    }

    \p{
      Some more bad ] text
    }

    \p{
      Skipped an ending brace here.
    |}
  in
  errors |> List.iter (fun (e : Core.Reporter.Message.t Asai.Diagnostic.t) -> Printf.printf "error: %s\n" (Asai.Diagnostic.string_of_text e.explanation.value));
  Printf.printf "parse_bad_result:\n%s\n\n" (Code.show bad)
