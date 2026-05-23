open Cohttp_lwt_unix
open Cmdliner

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let buf = Bytes.create n in
      really_input ic buf 0 n;
      Bytes.to_string buf)

let handle_request root _conn req _body =
  let uri = Request.uri req in
  let url_path = Uri.path uri in
  let accept =
    match Cohttp.Header.get (Request.headers req) "accept" with
    | Some v -> v
    | None -> "*/*"
  in
  let fmt = Lib.parse_accept accept in
  match Lib.resolve_path root url_path with
  | Some fs_path -> (
      let md_content = read_file fs_path in
      match fmt with
      | Lib.Html ->
          let body = Lib.markdown_to_html md_content in
          Server.respond_string ~status:`OK
            ~headers:(Cohttp.Header.of_list [ ("Content-Type", "text/html; charset=utf-8") ])
            ~body ()
      | Lib.Markdown ->
          Server.respond_string ~status:`OK
            ~headers:(Cohttp.Header.of_list [ ("Content-Type", "text/markdown; charset=utf-8") ])
            ~body:md_content ()
      | Lib.Plaintext ->
          let body = Lib.markdown_to_plaintext md_content in
          Server.respond_string ~status:`OK
            ~headers:(Cohttp.Header.of_list [ ("Content-Type", "text/plain; charset=utf-8") ])
            ~body ())
  | None -> (
      match Lib.directory_listing root url_path with
      | Some body ->
          Server.respond_string ~status:`OK
            ~headers:(Cohttp.Header.of_list [ ("Content-Type", "text/html; charset=utf-8") ])
            ~body ()
      | None ->
          Server.respond_string ~status:`Not_found
            ~headers:(Cohttp.Header.of_list [ ("Content-Type", "text/plain; charset=utf-8") ])
            ~body:"Not found\n" ())

let run root port =
  let root = match root with Some r -> r | None -> Sys.getcwd () in
  let root =
    if Filename.is_relative root then Filename.concat (Sys.getcwd ()) root
    else root
  in
  if not (Sys.file_exists root && Sys.is_directory root) then (
    Printf.eprintf "error: %s is not a directory\n" root;
    exit 1);
  Printf.printf "Serving %s on http://localhost:%d\n%!" root port;
  let callback = handle_request root in
  let server = Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ()) in
  Lwt_main.run server

(* === CLI === *)

let root_arg =
  let doc = "Directory to serve. Defaults to the current directory." in
  Arg.(value & (pos 0 (some string) None) & info [] ~docv:"DIR" ~doc)

let port_arg =
  let doc = "Port to listen on." in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let cmd =
  let doc = "Serve a folder of Markdown files over HTTP" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Starts an HTTP server that serves Markdown files from DIR (default: \
         current directory). The folder structure maps directly to URL paths. \
         An $(b,index.md) file is served at the root of its directory.";
      `P
        "The response format is determined by the $(b,Accept) header: \
         $(b,text/html) renders the Markdown as HTML, $(b,text/markdown) \
         returns the raw Markdown source, and $(b,text/plain) returns a \
         plain-text rendering.";
      `S Manpage.s_examples;
      `P "Serve the current directory on the default port:";
      `Pre "  tool-md-serve";
      `P "Serve a specific directory on port 3000:";
      `Pre "  tool-md-serve ./docs -p 3000";
      `P "Fetch a page as plain text:";
      `Pre {|  curl -H "Accept: text/plain" http://localhost:8080/readme|};
    ]
  in
  let info = Cmd.info "tool-md-serve" ~version:"1.0.0" ~doc ~man in
  Cmd.v info Term.(const run $ root_arg $ port_arg)

let () = exit (Cmd.eval cmd)
