open Core
module Store = Irmin_mem.KV.Make(Rep.Tree)
module Info = Irmin_unix.Info(Store.Info)

let config = Irmin_mem.config ()

open Lwt.Syntax

let main_branch config =
  let* repo = Store.Repo.v config in
  Store.main repo

let info message = Info.v ~author:"kentookura" "%s" message

let main =
  let addr = (User_addr "foo-0001") in
  let* t = main_branch config in
  let* () = Store.set_exn t ["foo-0001"] (Sem.empty_tree ~addr) ~info:(info "first commit") in
  let+ s = Store.get t ["foo-0001"] in
  assert (s = Sem.empty_tree ~addr)

let () = Lwt_main.run main
