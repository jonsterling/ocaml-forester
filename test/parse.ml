open Forester_core
open Forester_frontend.Parse

let emit _ = () (* ignore *)

let fatal _ = exit 1

let _ =
  Forester_core.Reporter.run ~emit ~fatal @@ fun () ->
  let good = Result.get_ok @@ parse_string {|
    \title{Good}
    \taxon{Test}
    \author{Testy}

    \p{
      This should parse correctly.
    }
    |}
  in
  Format.printf "parse_good_result:\n%s\n\n" (Code.show good)

let _ =
  Forester_core.Reporter.run ~emit ~fatal @@ fun () ->
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
  errors |> List.iter (fun (e : Forester_core.Reporter.Message.t Asai.Diagnostic.t) -> Format.printf "error: %s\n" (Asai.Diagnostic.string_of_text e.explanation.value));
  Format.printf "parse_bad_result:\n%s\n\n" (Code.show bad)

let _ =
  Forester_core.Reporter.run ~emit ~fatal @@ fun () ->
  (* Incomplete \p (brackets not unbalanced) should error *)
  let bad, errors = Result.get_error @@ parse_string {|
    \p{Keep me}
    \p
    |}
  in
  errors |> List.iter (fun (e : Forester_core.Reporter.Message.t Asai.Diagnostic.t) -> Format.printf "error: %s\n" (Asai.Diagnostic.string_of_text e.explanation.value));
  Format.printf "parse_bad_result:\n%s\n\n" (Code.show bad)

let _ =
  Forester_core.Reporter.run ~emit ~fatal @@ fun () ->
  (* Incomplete XML literal should error *)
  let bad, errors = Result.get_error @@ parse_string {|
    \p{Keep me}
    \<foo>[bar]{
      Incomplete fragment, needs another {} at the end.
    }
    |}
  in
  errors |> List.iter (fun (e : Forester_core.Reporter.Message.t Asai.Diagnostic.t) -> Format.printf "error: %s\n" (Asai.Diagnostic.string_of_text e.explanation.value));
  Format.printf "parse_bad_result:\n%s\n\n" (Code.show bad)
