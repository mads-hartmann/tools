(* === Types === *)

type response_format = Html | Markdown | Plaintext

(* === Path resolution === *)

(* Resolve a URL path to a filesystem path within the root directory.
   Returns None if the path escapes the root or the file doesn't exist. *)
let resolve_path root url_path =
  let stripped =
    if String.length url_path > 0 && url_path.[0] = '/' then
      String.sub url_path 1 (String.length url_path - 1)
    else url_path
  in
  (* Collapse ".." segments to prevent directory traversal *)
  let parts = String.split_on_char '/' stripped |> List.filter (fun s -> s <> "" && s <> ".") in
  let safe_parts =
    List.fold_left
      (fun acc part ->
        if part = ".." then (match acc with _ :: tl -> tl | [] -> [])
        else part :: acc)
      [] parts
    |> List.rev
  in
  let rel = String.concat Filename.dir_sep safe_parts in
  let candidate =
    if rel = "" then Filename.concat root "index.md"
    else
      let full = Filename.concat root rel in
      if Sys.file_exists full && Sys.is_directory full then
        Filename.concat full "index.md"
      else if Filename.check_suffix full ".md" then full
      else full ^ ".md"
  in
  if Sys.file_exists candidate && not (Sys.is_directory candidate) then
    Some candidate
  else None

(* === Accept header parsing === *)

(* Parse the Accept header and return the preferred response format.
   Walks types in order; first recognised type wins. Defaults to Html. *)
let parse_accept accept_header =
  let types =
    String.split_on_char ',' accept_header
    |> List.map (fun s ->
           match String.split_on_char ';' (String.trim s) with
           | t :: _ -> String.trim t
           | [] -> "")
  in
  let rec find = function
    | [] -> Html
    | t :: rest -> (
        match String.lowercase_ascii t with
        | "text/html" | "application/xhtml+xml" | "*/*" -> Html
        | "text/markdown" | "text/x-markdown" -> Markdown
        | "text/plain" -> Plaintext
        | _ -> find rest)
  in
  find types

(* === Rendering === *)

let markdown_to_html md_content =
  let doc = Cmarkit.Doc.of_string ~strict:false md_content in
  let fragment = Cmarkit_html.of_doc ~safe:false doc in
  Printf.sprintf
    {|<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<style>
  body { max-width: 800px; margin: 2rem auto; padding: 0 1rem;
         font-family: system-ui, sans-serif; line-height: 1.6; }
  pre { background: #f4f4f4; padding: 1rem; overflow-x: auto; border-radius: 4px; }
  code { background: #f4f4f4; padding: .1em .3em; border-radius: 3px; }
  pre code { background: none; padding: 0; }
</style>
</head>
<body>
%s
</body>
</html>|}
    fragment

let markdown_to_plaintext md_content =
  (* Render to HTML then strip tags — effective for most CommonMark content *)
  let doc = Cmarkit.Doc.of_string ~strict:false md_content in
  let html = Cmarkit_html.of_doc ~safe:false doc in
  let buf = Buffer.create (String.length html) in
  let in_tag = ref false in
  String.iter
    (fun c ->
      if c = '<' then in_tag := true
      else if c = '>' then in_tag := false
      else if not !in_tag then Buffer.add_char buf c)
    html;
  let text = Buffer.contents buf in
  let lines = String.split_on_char '\n' text in
  let collapsed =
    List.fold_left
      (fun (acc, prev_blank) line ->
        let blank = String.trim line = "" in
        if blank && prev_blank then (acc, true)
        else (line :: acc, blank))
      ([], false) lines
    |> fst |> List.rev
  in
  String.concat "\n" collapsed

(* === Directory listing === *)

let directory_listing root url_path =
  let fs_dir =
    let stripped =
      if String.length url_path > 0 && url_path.[0] = '/' then
        String.sub url_path 1 (String.length url_path - 1)
      else url_path
    in
    if stripped = "" then root else Filename.concat root stripped
  in
  if not (Sys.file_exists fs_dir && Sys.is_directory fs_dir) then None
  else
    let entries = Sys.readdir fs_dir |> Array.to_list |> List.sort String.compare in
    let base =
      if url_path = "/" then ""
      else
        let s = String.trim url_path in
        if String.length s > 0 && s.[String.length s - 1] = '/' then
          String.sub s 0 (String.length s - 1)
        else s
    in
    let items =
      List.filter_map
        (fun name ->
          let full = Filename.concat fs_dir name in
          if Sys.is_directory full then
            let href = base ^ "/" ^ name ^ "/" in
            Some (Printf.sprintf {|<li><a href="%s">%s/</a></li>|} href name)
          else if Filename.check_suffix name ".md" then
            let file_base = Filename.chop_suffix name ".md" in
            let href = base ^ "/" ^ file_base in
            Some (Printf.sprintf {|<li><a href="%s">%s</a></li>|} href name)
          else None)
        entries
    in
    let body =
      Printf.sprintf
        {|<!DOCTYPE html>
<html lang="en">
<head><meta charset="utf-8"><title>Index of %s</title>
<style>body{max-width:800px;margin:2rem auto;padding:0 1rem;font-family:system-ui,sans-serif}</style>
</head>
<body>
<h1>Index of %s</h1>
<ul>%s</ul>
</body></html>|}
        url_path url_path (String.concat "\n" items)
    in
    Some body
