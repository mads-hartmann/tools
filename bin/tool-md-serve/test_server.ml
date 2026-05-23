(* Test suite for the pure functions in Lib.
   Fixtures live in fixtures/ next to this source file. *)

(* Accept the fixtures directory as argv[1] (passed by the dune rule).
   Falls back to a path relative to __FILE__ for direct invocation. *)
let fixtures_dir =
  if Array.length Sys.argv > 1 then Sys.argv.(1)
  else Filename.concat (Filename.dirname __FILE__) "fixtures"

(* === Test harness === *)

let tests_run = ref 0
let tests_passed = ref 0

let test name f =
  incr tests_run;
  try
    f ();
    incr tests_passed;
    Printf.printf "  ✓ %s\n" name
  with e ->
    Printf.printf "  ✗ %s: %s\n" name (Printexc.to_string e)

let assert_eq ~label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let assert_some ~label = function
  | Some v -> v
  | None -> failwith (Printf.sprintf "%s: expected Some, got None" label)

let assert_none ~label = function
  | None -> ()
  | Some v -> failwith (Printf.sprintf "%s: expected None, got Some %S" label v)

let contains ~needle haystack =
  let nlen = String.length needle and hlen = String.length haystack in
  if nlen = 0 then true
  else if nlen > hlen then false
  else
    let found = ref false in
    for i = 0 to hlen - nlen do
      if not !found &&
         String.sub haystack i nlen = needle then
        found := true
    done;
    !found

let assert_contains ~label needle haystack =
  if not (contains ~needle haystack) then
    failwith (Printf.sprintf "%s: %S not found in output" label needle)

(* === parse_accept === *)

let () =
  print_endline "Testing parse_accept:";

  test "text/html -> Html" (fun () ->
    assert_eq ~label:"format" "Html"
      (match Lib.parse_accept "text/html" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "text/markdown -> Markdown" (fun () ->
    assert_eq ~label:"format" "Markdown"
      (match Lib.parse_accept "text/markdown" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "text/x-markdown -> Markdown" (fun () ->
    assert_eq ~label:"format" "Markdown"
      (match Lib.parse_accept "text/x-markdown" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "text/plain -> Plaintext" (fun () ->
    assert_eq ~label:"format" "Plaintext"
      (match Lib.parse_accept "text/plain" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "*/* -> Html (default)" (fun () ->
    assert_eq ~label:"format" "Html"
      (match Lib.parse_accept "*/*" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "empty -> Html (default)" (fun () ->
    assert_eq ~label:"format" "Html"
      (match Lib.parse_accept "" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "unknown type -> Html (default)" (fun () ->
    assert_eq ~label:"format" "Html"
      (match Lib.parse_accept "application/json" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "q-value parameters are stripped" (fun () ->
    assert_eq ~label:"format" "Markdown"
      (match Lib.parse_accept "text/markdown;q=0.9" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "first matching type wins in multi-value header" (fun () ->
    (* text/plain comes first, so Plaintext wins even though text/html follows *)
    assert_eq ~label:"format" "Plaintext"
      (match Lib.parse_accept "text/plain, text/html" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"));

  test "case-insensitive matching" (fun () ->
    assert_eq ~label:"format" "Html"
      (match Lib.parse_accept "Text/HTML" with
       | Lib.Html -> "Html" | Lib.Markdown -> "Markdown" | Lib.Plaintext -> "Plaintext"))

(* === resolve_path === *)

let () =
  print_endline "\nTesting resolve_path:";

  test "/ resolves to index.md" (fun () ->
    let path = assert_some ~label:"/" (Lib.resolve_path fixtures_dir "/") in
    assert_eq ~label:"path" (Filename.concat fixtures_dir "index.md") path);

  test "empty path resolves to index.md" (fun () ->
    let path = assert_some ~label:"empty" (Lib.resolve_path fixtures_dir "") in
    assert_eq ~label:"path" (Filename.concat fixtures_dir "index.md") path);

  test "/guide resolves to guide.md" (fun () ->
    let path = assert_some ~label:"/guide" (Lib.resolve_path fixtures_dir "/guide") in
    assert_eq ~label:"path" (Filename.concat fixtures_dir "guide.md") path);

  test "/about resolves to about.md" (fun () ->
    let path = assert_some ~label:"/about" (Lib.resolve_path fixtures_dir "/about") in
    assert_eq ~label:"path" (Filename.concat fixtures_dir "about.md") path);

  test "/reference resolves to reference/index.md (directory index)" (fun () ->
    let path = assert_some ~label:"/reference" (Lib.resolve_path fixtures_dir "/reference") in
    assert_eq ~label:"path"
      (Filename.concat (Filename.concat fixtures_dir "reference") "index.md") path);

  test "/reference/ resolves to reference/index.md (trailing slash)" (fun () ->
    let path = assert_some ~label:"/reference/" (Lib.resolve_path fixtures_dir "/reference/") in
    assert_eq ~label:"path"
      (Filename.concat (Filename.concat fixtures_dir "reference") "index.md") path);

  test "/reference/api resolves to reference/api.md" (fun () ->
    let path = assert_some ~label:"/reference/api" (Lib.resolve_path fixtures_dir "/reference/api") in
    assert_eq ~label:"path"
      (Filename.concat (Filename.concat fixtures_dir "reference") "api.md") path);

  test "missing file returns None" (fun () ->
    assert_none ~label:"/nonexistent"
      (Lib.resolve_path fixtures_dir "/nonexistent"));

  test "directory traversal is blocked" (fun () ->
    (* /../ should be collapsed and not escape the root *)
    assert_none ~label:"traversal"
      (Lib.resolve_path fixtures_dir "/../etc/passwd"))

(* === markdown_to_html === *)

let () =
  print_endline "\nTesting markdown_to_html:";

  test "wraps output in a full HTML page" (fun () ->
    let html = Lib.markdown_to_html "# Hello" in
    assert_contains ~label:"doctype" "<!DOCTYPE html>" html;
    assert_contains ~label:"body open" "<body>" html;
    assert_contains ~label:"body close" "</body>" html);

  test "renders heading" (fun () ->
    let html = Lib.markdown_to_html "# Hello" in
    assert_contains ~label:"h1" "<h1>Hello</h1>" html);

  test "renders paragraph" (fun () ->
    let html = Lib.markdown_to_html "Hello world" in
    assert_contains ~label:"p" "<p>Hello world</p>" html);

  test "renders bold" (fun () ->
    let html = Lib.markdown_to_html "**bold**" in
    assert_contains ~label:"strong" "<strong>bold</strong>" html);

  test "renders code block" (fun () ->
    let html = Lib.markdown_to_html "```\nlet x = 1\n```" in
    assert_contains ~label:"pre" "<pre>" html;
    assert_contains ~label:"code" "<code>" html);

  test "renders fixture index.md without error" (fun () ->
    let ic = open_in (Filename.concat fixtures_dir "index.md") in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    let html = Lib.markdown_to_html content in
    assert_contains ~label:"h1" "<h1>" html)

(* === markdown_to_plaintext === *)

let () =
  print_endline "\nTesting markdown_to_plaintext:";

  test "strips heading tags" (fun () ->
    let text = Lib.markdown_to_plaintext "# Hello" in
    assert_contains ~label:"text" "Hello" text;
    if contains ~needle:"<h1>" text then
      failwith "HTML tags should be stripped");

  test "strips bold tags" (fun () ->
    let text = Lib.markdown_to_plaintext "**bold**" in
    assert_contains ~label:"text" "bold" text;
    if contains ~needle:"<strong>" text then
      failwith "HTML tags should be stripped");

  test "preserves text content" (fun () ->
    let text = Lib.markdown_to_plaintext "Hello world" in
    assert_contains ~label:"text" "Hello world" text);

  test "collapses consecutive blank lines" (fun () ->
    let text = Lib.markdown_to_plaintext "# A\n\n\n\n# B" in
    (* Should not have more than one consecutive blank line *)
    if contains ~needle:"\n\n\n" text then
      failwith "consecutive blank lines not collapsed")

(* === directory_listing === *)

let () =
  print_endline "\nTesting directory_listing:";

  test "root listing contains known files" (fun () ->
    let html = assert_some ~label:"root listing"
      (Lib.directory_listing fixtures_dir "/") in
    assert_contains ~label:"guide link" "/guide" html;
    assert_contains ~label:"about link" "/about" html);

  test "root listing contains subdirectory link" (fun () ->
    let html = assert_some ~label:"root listing"
      (Lib.directory_listing fixtures_dir "/") in
    assert_contains ~label:"reference dir" "reference/" html);

  test "subdirectory listing contains nested files" (fun () ->
    let html = assert_some ~label:"reference listing"
      (Lib.directory_listing fixtures_dir "/reference") in
    assert_contains ~label:"api link" "/reference/api" html);

  test "listing hrefs have no double slashes" (fun () ->
    let html = assert_some ~label:"root listing"
      (Lib.directory_listing fixtures_dir "/") in
    if contains ~needle:"href=\"//" html then
      failwith "double slash found in href");

  test "non-existent directory returns None" (fun () ->
    match Lib.directory_listing fixtures_dir "/no-such-dir" with
    | None -> ()
    | Some _ -> failwith "expected None for missing directory");

  test "file path (not a directory) returns None" (fun () ->
    match Lib.directory_listing fixtures_dir "/guide" with
    | None -> ()
    | Some _ -> failwith "expected None for file path")

(* === Summary === *)

let () =
  print_endline "";
  Printf.printf "Results: %d/%d tests passed\n" !tests_passed !tests_run;
  if !tests_passed <> !tests_run then exit 1
