open Types
module X = Xmlm

type printer = X.output -> unit 

class type env = 
  object 
    method transclude : addr -> printer
  end

module H = 
struct
  let text txt : printer = 
    fun out -> 
      X.output out @@ `Data txt

  let iter printer xs : printer =
    fun out -> 
    xs |> List.iter @@ fun x -> 
    printer x out

  let tag name attrs body : printer = 
    let attrs' = attrs |> List.map @@ fun (k,v) -> (("", k), v) in 
    fun out ->
      X.output out (`El_start (("", name), attrs'));
      body out; 
      X.output out `El_end

  let a ~href body : printer = 
    tag "a" ["href", href] body
end

let rec render (env : env) : Sem.t -> printer =
  function 
  | Sem.Text txt -> 
    let txt = String.trim txt in 
    if String.length txt > 0 then 
      H.text @@ txt 
    else 
      fun _ -> ()
  | Sem.Wikilink (title, addr) -> 
    H.a ~href:addr @@ render env title
  | Sem.Seq xs -> 
    xs |> H.iter (render env)
  | Sem.Tag (name, attrs, body) -> 
    H.tag name attrs @@ render env body
  | Sem.Transclude addr -> 
    env#transclude addr
