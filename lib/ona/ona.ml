(** Ona: Shared library for Ona environment tools *)

(** Find the Ona CLI in PATH or common locations *)
let find_cli () =
  (* Try 'which gitpod' first *)
  let ic = Unix.open_process_in "which gitpod 2>/dev/null" in
  let result =
    try Some (input_line ic |> String.trim)
    with End_of_file -> None
  in
  let _ = Unix.close_process_in ic in
  match result with
  | Some path when path <> "" && Sys.file_exists path -> Some path
  | _ ->
      (* Fallback to common locations *)
      let paths = ["/usr/local/bin/gitpod"; "/usr/bin/gitpod"; "/opt/homebrew/bin/gitpod"] in
      List.find_opt Sys.file_exists paths

(** Represents an Ona environment *)
type env = {
  id : string;
  nickname : string;
  checkout_location : string option;
}

(** Result type for command execution *)
type command_result = {
  stdout : string;
  exit_code : int;
}

(** Run a command and return its stdout and exit code *)
let run_command cmd args =
  let cmd_str = String.concat " " (cmd :: args) in
  let ic = Unix.open_process_in cmd_str in
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  let status = Unix.close_process_in ic in
  let exit_code = match status with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED _ -> 128
    | Unix.WSTOPPED _ -> 128
  in
  { stdout = Buffer.contents buf; exit_code }

(** Extract checkout location from environment JSON *)
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
    |> Option.some
  with _ -> None

(** Extract nickname from environment JSON.
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

(** Error type for environment operations *)
type error =
  | Cli_not_found
  | Cli_error of int * string
  | Parse_error of string

(** Parse environment list JSON. Returns (envs, error option) *)
let parse_envs ?(include_checkout_location = false) json_str =
  if String.trim json_str = "" then
    ([], Some (Parse_error "Empty response from CLI"))
  else
    try
      let json = Yojson.Basic.from_string json_str in
      let open Yojson.Basic.Util in
      let envs = json
        |> to_list
        |> List.map (fun env_json ->
               {
                 id = env_json |> member "id" |> to_string;
                 nickname = extract_nickname env_json;
                 checkout_location =
                   if include_checkout_location then extract_checkout_location env_json
                   else None;
               })
      in
      (envs, None)
    with
    | Yojson.Json_error msg -> ([], Some (Parse_error msg))
    | e -> ([], Some (Parse_error (Printexc.to_string e)))

(** List running Ona environments. Returns (envs, error option) *)
let list_environments_result ?(include_checkout_location = false) () =
  match find_cli () with
  | None -> ([], Some Cli_not_found)
  | Some cli_path ->
      let result = run_command cli_path [ "environment"; "list"; "-o"; "json"; "--running-only"; "2>/dev/null" ] in
      if result.exit_code <> 0 then
        ([], Some (Cli_error (result.exit_code, result.stdout)))
      else
        parse_envs ~include_checkout_location result.stdout

(** List running Ona environments. Legacy API that returns empty list on error. *)
let list_environments ?(include_checkout_location = false) () =
  fst (list_environments_result ~include_checkout_location ())

(** Get a single environment by ID. Returns Some env if it exists and is running, None otherwise. *)
let get_environment id =
  match find_cli () with
  | None -> None
  | Some cli_path ->
      let result = run_command cli_path [ "environment"; "get"; id; "-o"; "json"; "2>/dev/null" ] in
      if result.exit_code <> 0 then None
      else
        try
          let json = Yojson.Basic.from_string result.stdout in
          let open Yojson.Basic.Util in
          (* The CLI returns a list even for a single environment *)
          match json |> to_list with
          | [] -> None
          | env_json :: _ ->
              let phase =
                try env_json |> member "status" |> member "phase" |> to_string
                with _ -> ""
              in
              if phase = "ENVIRONMENT_PHASE_RUNNING" then
                Some {
                  id = env_json |> member "id" |> to_string;
                  nickname = extract_nickname env_json;
                  checkout_location = None;
                }
              else
                None
        with _ -> None

(** Format error for display *)
let error_to_string = function
  | Cli_not_found -> "Gitpod CLI not found. Ensure 'gitpod' is in your PATH."
  | Cli_error (code, output) ->
      Printf.sprintf "CLI exited with code %d: %s" code (String.trim output)
  | Parse_error msg -> Printf.sprintf "Failed to parse CLI output: %s" msg
