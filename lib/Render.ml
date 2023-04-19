open Types

type printer = Xmlm.output -> unit 

class type env = 
  object 
    method transclude : addr -> printer
  end

module Printer =
struct 
  let text txt : printer = 
    fun out -> 
      Xmlm.output out @@ `Data txt

  let trimmedText (txt : string) : printer =
    let txt = String.trim txt in 
    if String.length txt > 0 then 
      text @@ txt 
    else 
      fun _ -> ()

  let iter printer xs : printer =
    fun out -> 
    xs |> List.iter @@ fun x -> 
    printer x out

  let seq ps : printer = 
    iter (fun p -> p) ps
end

module Html = 
struct
  let tag name attrs body : printer = 
    let attrs' = attrs |> List.map @@ fun (k,v) -> (("", k), v) in 
    fun out ->
      Xmlm.output out (`El_start (("", name), attrs'));
      body out; 
      Xmlm.output out `El_end

  let a ~href body : printer = 
    tag "a" ["href", href] body
end

let rec renderMathMode (env : env) : Sem.t -> string = 
  function 
  | Sem.Text txt -> String.trim txt
  | Sem.Seq xs -> List.fold_right (fun y r -> renderMathMode env y ^ r) xs ""
  | Sem.Math x -> renderMathMode env x 
  | Sem.Tag (name, [], args) ->
    "\\" ^ name ^ 
    begin 
      match args with 
      | [] -> "{}"
      | _ -> List.fold_right (fun x r -> renderMathArg env x ^ r) args ""
    end
  | _ -> failwith "renderMathMode"

and renderMathArg (env : env) (arg : Sem.t) : string = 
  "{" ^ renderMathMode env arg ^ "}"

let rec render (env : env) : Sem.t -> printer = 
  function 
  | Sem.Text txt -> 
    Printer.trimmedText txt 
  | Sem.Seq xs ->
    xs |> Printer.iter (render env)
  | Sem.Math body -> 
    Printer.text @@ "\\(" ^ renderMathMode env body ^ "\\)"
  | Sem.Wikilink (title, addr) -> 
    Html.a ~href:addr @@ render env title
  | Sem.Tag (name, attrs, xs) -> 
    xs |> Printer.iter (render env) |> Html.tag name attrs
  | Sem.Transclude addr -> 
    env#transclude addr
