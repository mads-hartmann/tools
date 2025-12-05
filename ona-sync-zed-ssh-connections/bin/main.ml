(** ona-sync-zed-ssh-connections: Syncs Ona environments to Zed's ssh_connections config *)

let zed_config_path =
  Filename.concat (Sys.getenv "HOME") ".config/zed/settings.json"

(** Convert an Ona environment to an ssh_connection JSON object *)
let env_to_ssh_connection (env : Ona.env) =
  let host = Printf.sprintf "%s.gitpod.environment" env.id in
  let checkout_location = Option.value env.checkout_location ~default:"workspace" in
  let path = Printf.sprintf "/workspace/%s" checkout_location in
  `Assoc
    [
      ("host", `String host);
      ("nickname", `String env.nickname);
      ("args", `List []);
      ("projects", `List [ `Assoc [ ("paths", `List [ `String path ]) ] ]);
    ]

(** Check if an ssh_connection is an Ona/Gitpod connection *)
let is_ona_connection json =
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

(** Parse existing ssh_connections to find non-Ona ones *)
let get_non_ona_connections content =
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
         json |> to_list |> List.filter (fun c -> not (is_ona_connection c))
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
    Returns Some (ona_count, non_ona_count) on success, None on failure. *)
let update_config ona_envs =
  match read_config () with
  | None ->
      Printf.eprintf "Zed config not found at %s\n%!" zed_config_path;
      None
  | Some content ->
      let non_ona = get_non_ona_connections content in
      let ona_connections = List.map env_to_ssh_connection ona_envs in
      let all_connections = non_ona @ ona_connections in
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
          Some (List.length ona_connections, List.length non_ona))

(** Main sync function *)
let sync () =
  Printf.printf "Syncing Ona environments...\n%!";
  let envs = Ona.list_environments ~include_checkout_location:true () in
  Printf.printf "Found %d running environment(s)\n%!" (List.length envs);
  List.iter
    (fun (env : Ona.env) ->
      let checkout = Option.value env.checkout_location ~default:"workspace" in
      Printf.printf "  - %s [%s] (%s)\n%!" env.id env.nickname checkout)
    envs;
  match update_config envs with
  | Some (ona_count, non_ona_count) ->
      Printf.printf "Updated Zed config (%d Ona, %d non-Ona connections)\n%!"
        ona_count non_ona_count
  | None ->
      Printf.printf "Failed to update Zed config\n%!"

(** Main entry point *)
let () =
  Printf.printf "ona-sync-zed-ssh-connections starting...\n%!";
  Printf.printf "Config path: %s\n%!" zed_config_path;
  while true do
    sync ();
    Printf.printf "Sleeping for 30 seconds...\n%!";
    Unix.sleep 30
  done
