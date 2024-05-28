open Core
open Lexing

module I = Grammar.MenhirInterpreter

(* debugging helpers *)
let string_of_token token =
  match token with
  | Grammar.LBRACE -> "LBRACE"
  | Grammar.RBRACE -> "RBRACE"
  | Grammar.LSQUARE -> "LSQUARE"
  | Grammar.RSQUARE -> "RSQUARE"
  | Grammar.LPAREN -> "LPAREN"
  | Grammar.RPAREN -> "RPAREN"
  | Grammar.HASH_LBRACE -> "HASH_LBRACE"
  | Grammar.HASH_HASH_LBRACE -> "HASH_HASH_LBRACE"
  | Grammar.WHITESPACE w -> w
  | Grammar.TEXT s -> s
  | Grammar.EOF -> "EOF"
  | Grammar.IDENT s -> Format.sprintf "IDENT(%s)" s
  | _ -> "<unimplemented>"

let char_of_token token =
  match token with
  | Grammar.LBRACE -> '{'
  | Grammar.RBRACE -> '}'
  | Grammar.LSQUARE -> '['
  | Grammar.RSQUARE -> ']'
  | Grammar.LPAREN -> '('
  | Grammar.RPAREN -> ')'
  | Grammar.HASH_LBRACE -> '#'
  | Grammar.HASH_HASH_LBRACE -> '#'
  | _ -> 'x'

(* drive the parser to the next InputNeeded checkpoint *)
let rec resumes checkpoint =
  match checkpoint with
  | I.InputNeeded env -> I.input_needed env
  | I.Shifting _ | I.AboutToReduce _ -> resumes @@ I.resume checkpoint
  | _ -> assert false

(* strategy: whenever we hit an unexpected closing delimiter, we look for a matching opening delimiter in the past
   if we find one, close all intermediate (hanging) delimiters and then continue parsing
   otherwise just continue parsing
   if we hit a premature EOF, try to close all delimiters, and if that fails return the last good parse
   (on each token, we test if the ending here would have produced a valid parse) *)
let try_parse lexbuf =
  let rec fail bracketing last_token last_accept before supplier chkpt =
    match chkpt with
    | I.HandlingError env ->
      let loc = Asai.Range.of_lexbuf lexbuf in
      Reporter.emitf ~loc Parse_error "syntax error, unexpected `%s`\n" (Lexing.lexeme lexbuf);
      begin
        match last_token with
        | Grammar.RPAREN
        | Grammar.RSQUARE
        | Grammar.RBRACE
          ->
          begin
            match List.find_index (fun c -> c = last_token) bracketing with
            | Some i ->
              (* try to find a small enclosing scope *)
              let consume = List.to_seq bracketing |> Seq.take (i + 1) in
              let remaining = List.to_seq bracketing |> Seq.drop i |> List.of_seq in
              let continue = Seq.fold_left (fun acc t -> resumes @@ I.offer acc (t, lexbuf.lex_curr_p, lexbuf.lex_curr_p)) before consume in
              run remaining last_token last_accept before supplier continue
            | None ->
              (* ignore this token and move on *)
              run bracketing Grammar.EOF last_accept before supplier before
          end
        | Grammar.EOF ->
          if not @@ List.is_empty bracketing then
            (* have hanging delimiters to close *)
            let continue = List.fold_left (fun acc t -> resumes @@ I.offer acc (t, lexbuf.lex_curr_p, lexbuf.lex_curr_p)) before bracketing in
            run [] last_token last_accept before supplier continue
          else
            (* can't continue, give up and use last_accept *)
            run [] last_token last_accept before supplier last_accept
        | _ ->
          (* ignore this token and move on *)
          run bracketing Grammar.EOF last_accept before supplier before
      end
    | _ -> Reporter.fatal Parse_error "unreachable parser state"

  and run bracketing last_token last_accept last_input_needed supplier checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
      (* last_token has been accepted, update bracketing *)
      let bracketing =
        match last_token with
        | Grammar.RPAREN
        | Grammar.RSQUARE
        | Grammar.RBRACE
          -> assert (List.hd bracketing = last_token); List.tl bracketing
        | _ -> bracketing
      in
      (* get new token *)
      let token, start, end_ = supplier () in
      let bracketing = match token with
        | Grammar.LPAREN -> Grammar.RPAREN :: bracketing
        | Grammar.LSQUARE -> Grammar.RSQUARE :: bracketing
        | Grammar.LBRACE
        | Grammar.HASH_LBRACE
        | Grammar.HASH_HASH_LBRACE
          -> Grammar.RBRACE :: bracketing
        | _ -> bracketing
      in
      (* check if it's possible to end parsing here, update last_accept *)
      let la =
        if I.acceptable checkpoint Grammar.EOF start
        then checkpoint
        else last_accept
      in
      run bracketing token la checkpoint supplier @@ I.offer checkpoint (token, start, end_)
    | I.Accepted v -> v
    | I.Rejected
    | I.HandlingError _ ->
      fail bracketing last_token last_accept last_input_needed supplier checkpoint
    | I.Shifting _
    | I.AboutToReduce _ ->
      run bracketing last_token last_accept last_input_needed supplier @@ I.resume checkpoint
  in
  let checkpoint = Grammar.Incremental.main lexbuf.lex_curr_p in
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  run [] Grammar.EOF checkpoint checkpoint supplier checkpoint

let maybe_with_errors (f : unit -> 'a) : ('a, 'a * 'b list) result =
  let errors = ref [] in
  let result =
    Reporter.map_diagnostic (fun d -> errors := d :: !errors; d) @@ fun () ->
    f ()
  in
  match !errors with
  | [] -> Result.ok result
  | errs -> Result.error (result, List.rev errs)

let parse_channel filename ch =
  Reporter.tracef "when parsing file `%s`" filename @@ fun () ->
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  maybe_with_errors (fun () -> try_parse lexbuf)

let parse_file fp =
  let filename = Eio.Path.native_exn fp in
  let ch = open_in filename in
  Fun.protect ~finally:(fun _ -> close_in ch) @@ fun _ ->
  parse_channel filename ch

let parse_string str =
  Reporter.tracef "when parsing string" @@ fun () ->
  let lexbuf = Lexing.from_string str in
  maybe_with_errors (fun () -> try_parse lexbuf)
