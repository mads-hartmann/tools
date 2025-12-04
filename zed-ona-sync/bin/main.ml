(** zed-ona-sync: Syncs Gitpod environments to Zed's ssh_connections config *)

let gitpod_cli = "/usr/local/bin/gitpod"
let zed_config_path =
  Filename.concat (Sys.getenv "HOME") ".config/zed/settings.json"

(** Represents a Gitpod environment *)
type gitpod_env = {
  id : string;
  checkout_location : string;
  nickname : string;
}

(** Run a command and return its stdout *)
let run_command cmd args =
  let cmd_str = String.concat " " (cmd :: args) in
  let ic = Unix.open_process_in cmd_str in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Buffer.contents buf

(** Extract checkout location from gitpod environment JSON *)
let extract_checkout_location json =
  let open Yojson.Basic.Util in
  try
    json
    |> member "spec"
    |> member "content"
    |> member "initializer"
    |> member "specs"
    |> to_list
    |> List.hd
    |> member "git"
    |> member "checkoutLocation"
    |> to_string
  with _ -> "workspace"

(** Extract nickname from gitpod environment JSON.
    Uses metadata.name if set, otherwise falls back to status.content.git.branch *)
let extract_nickname json =
  let open Yojson.Basic.Util in
  let metadata_name =
    try
      match json |> member "metadata" |> member "name" with
      | `Null -> None
      | `String "" -> None
      | `String s -> Some s
      | _ -> None
    with _ -> None
  in
  match metadata_name with
  | Some name -> name
  | None ->
      (try
         json
         |> member "status"
         |> member "content"
         |> member "git"
         |> member "branch"
         |> to_string
       with _ -> "unknown")

(** Parse gitpod environment list JSON *)
let parse_gitpod_envs json_str =
  try
    let json = Yojson.Basic.from_string json_str in
    let open Yojson.Basic.Util in
    json
    |> to_list
    |> List.map (fun env ->
           {
             id = env |> member "id" |> to_string;
             checkout_location = extract_checkout_location env;
             nickname = extract_nickname env;
           })
  with _ -> []

(** List running Gitpod environments *)
let list_gitpod_environments () =
  let output =
    run_command gitpod_cli [ "environment"; "list"; "-o"; "json"; "--running-only"; "2>/dev/null" ]
  in
  parse_gitpod_envs output

(** Convert a gitpod environment to an ssh_connection JSON object *)
let env_to_ssh_connection env =
  let host = Printf.sprintf "%s.gitpod.environment" env.id in
  let path = Printf.sprintf "/workspace/%s" env.checkout_location in
  `Assoc
    [
      ("host", `String host);
      ("nickname", `String env.nickname);
      ("args", `List []);
      ("projects", `List [ `Assoc [ ("paths", `List [ `String path ]) ] ]);
    ]

(** Check if an ssh_connection is a gitpod connection *)
let is_gitpod_connection json =
  let open Yojson.Basic.Util in
  try
    let host = json |> member "host" |> to_string in
    String.length host > 19
    && String.sub host (String.length host - 19) 19 = ".gitpod.environment"
  with _ -> false

(** Find the boundaries of the ssh_connections array in the config string.
    Returns (start_pos, end_pos) of the array (including brackets). *)
let find_ssh_connections_array content =
  (* Find "ssh_connections" key *)
  let key = "\"ssh_connections\"" in
  match String.split_on_char '"' content with
  | _ ->
      (* Use a simple search approach *)
      let rec find_key pos =
        if pos >= String.length content - String.length key then None
        else if String.sub content pos (String.length key) = key then Some pos
        else find_key (pos + 1)
      in
      (match find_key 0 with
      | None -> None
      | Some key_pos ->
          (* Find the opening bracket after the key *)
          let after_key = key_pos + String.length key in
          let rec find_bracket pos =
            if pos >= String.length content then None
            else
              match content.[pos] with
              | '[' -> Some pos
              | ':' | ' ' | '\t' | '\n' | '\r' -> find_bracket (pos + 1)
              | _ -> None
          in
          (match find_bracket after_key with
          | None -> None
          | Some bracket_start ->
              (* Find matching closing bracket, accounting for nesting *)
              let rec find_closing pos depth in_string escape =
                if pos >= String.length content then None
                else if escape then find_closing (pos + 1) depth in_string false
                else
                  match content.[pos] with
                  | '\\' when in_string ->
                      find_closing (pos + 1) depth in_string true
                  | '"' -> find_closing (pos + 1) depth (not in_string) false
                  | '[' when not in_string ->
                      find_closing (pos + 1) (depth + 1) in_string false
                  | ']' when not in_string ->
                      if depth = 1 then Some pos
                      else find_closing (pos + 1) (depth - 1) in_string false
                  | _ -> find_closing (pos + 1) depth in_string false
              in
              (match find_closing bracket_start 0 false false with
              | None -> None
              | Some bracket_end -> Some (bracket_start, bracket_end))))

(** Read the Zed config file *)
let read_config () =
  if Sys.file_exists zed_config_path then (
    let ic = open_in zed_config_path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Some s)
  else None

(** Write the Zed config file *)
let write_config content =
  let oc = open_out zed_config_path in
  output_string oc content;
  close_out oc

(** Strip // comments from a string for JSON parsing only *)
let strip_comments str =
  let lines = String.split_on_char '\n' str in
  let stripped =
    List.map
      (fun line ->
        match String.index_opt line '/' with
        | None -> line
        | Some i ->
            if i + 1 < String.length line && line.[i + 1] = '/' then
              String.sub line 0 i
            else line)
      lines
  in
  String.concat "\n" stripped

(** Parse existing ssh_connections to find non-gitpod ones *)
let get_non_gitpod_connections content =
  match find_ssh_connections_array content with
  | None -> []
  | Some (start_pos, end_pos) ->
      let array_str =
        String.sub content start_pos (end_pos - start_pos + 1)
      in
      let stripped = strip_comments array_str in
      (try
         let json = Yojson.Basic.from_string stripped in
         let open Yojson.Basic.Util in
         json |> to_list |> List.filter (fun c -> not (is_gitpod_connection c))
       with _ -> [])

(** Format the ssh_connections array with proper indentation *)
let format_ssh_connections connections =
  let json = `List connections in
  (* Pretty print with 2-space indent, then adjust for the config file *)
  let formatted = Yojson.Basic.pretty_to_string ~std:true json in
  (* Add extra indentation for each line (2 spaces for being inside root object) *)
  let lines = String.split_on_char '\n' formatted in
  let indented =
    List.mapi
      (fun i line -> if i = 0 then line else "  " ^ line)
      lines
  in
  String.concat "\n" indented

(** Update the Zed config with new ssh_connections.
    Returns Some (gitpod_count, non_gitpod_count) on success, None on failure. *)
let update_config gitpod_envs =
  match read_config () with
  | None ->
      Printf.eprintf "Zed config not found at %s\n%!" zed_config_path;
      None
  | Some content ->
      let non_gitpod = get_non_gitpod_connections content in
      let gitpod_connections = List.map env_to_ssh_connection gitpod_envs in
      let all_connections = non_gitpod @ gitpod_connections in
      (match find_ssh_connections_array content with
      | None ->
          Printf.eprintf "No ssh_connections found in config\n%!";
          None
      | Some (start_pos, end_pos) ->
          let new_array = format_ssh_connections all_connections in
          let before = String.sub content 0 start_pos in
          let after =
            String.sub content (end_pos + 1)
              (String.length content - end_pos - 1)
          in
          let new_content = before ^ new_array ^ after in
          write_config new_content;
          Some (List.length gitpod_connections, List.length non_gitpod))

(** Main sync function *)
let sync () =
  Printf.printf "Syncing Gitpod environments...\n%!";
  let envs = list_gitpod_environments () in
  Printf.printf "Found %d running environment(s)\n%!" (List.length envs);
  List.iter
    (fun env ->
      Printf.printf "  - %s [%s] (%s)\n%!" env.id env.nickname env.checkout_location)
    envs;
  match update_config envs with
  | Some (gitpod_count, non_gitpod_count) ->
      Printf.printf "Updated Zed config (%d gitpod, %d non-gitpod connections)\n%!"
        gitpod_count non_gitpod_count
  | None ->
      Printf.printf "Failed to update Zed config\n%!"

(** Main entry point *)
let () =
  Printf.printf "zed-ona-sync starting...\n%!";
  Printf.printf "Config path: %s\n%!" zed_config_path;
  while true do
    sync ();
    Printf.printf "Sleeping for 30 seconds...\n%!";
    Unix.sleep 30
  done
