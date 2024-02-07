open Core
module E = Render_effect.Perform

let rec expand_title_with_parents (tree : Sem.tree) title =
  match tree.fm.parent with
  | None ->
    title
  | Some parent_addr ->
    match E.get_doc parent_addr with
    | None ->
      title
    | Some parent ->
      let chevron = [Range.locate_opt None @@ Sem.Text " › "] in
      let parent_title = parent.fm.title |> Option.map @@ expand_title_with_parents parent in
      let parent_link =
        [Range.locate_opt None @@
         Sem.Link {dest = parent_addr; modifier = Some `Sentence_case; title = parent_title}]
      in
      parent_link @ chevron @ title
