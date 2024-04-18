open Core
module E = Render_effect.Perform

let rec expand_title_with_parents ?(ancestors = []) (fm : Sem.frontmatter) title =
  match fm.designated_parent with
  | Some parent_addr when not @@ List.mem parent_addr ancestors ->
    begin
      match E.get_doc parent_addr with
      | None ->
        title
      | Some parent ->
        let chevron = [Range.locate_opt None @@ Sem.Text " › "] in
        let parent_title = parent.fm.title |> Option.map @@ expand_title_with_parents parent.fm in
        let parent_link =
          [Range.locate_opt None @@
           Sem.Link (parent_addr, parent_title,  Sentence_case)]
        in
        parent_link @ chevron @ title
    end
  | _ -> title
