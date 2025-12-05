(** ona-sync-zed-ssh-connections: Syncs Ona environments to Zed's ssh_connections config *)

open Cmdliner

let default_zed_config_path =
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
let read_config config_path =
  if Sys.file_exists config_path then (
    let ic = open_in config_path in
    let n = in_channel_length ic in
    let s = really_input_string ic n in
    close_in ic;
    Some s)
  else None

(** Write the Zed config file *)
let write_config config_path content =
  let oc = open_out config_path in
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
let update_config config_path ona_envs =
  match read_config config_path with
  | None ->
      Printf.eprintf "Zed config not found at %s\n%!" config_path;
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
          write_config config_path new_content;
          Some (List.length ona_connections, List.length non_ona))

(** Main sync function *)
let sync config_path =
  Printf.printf "Syncing Ona environments...\n%!";
  let envs = Ona.list_environments ~include_checkout_location:true () in
  Printf.printf "Found %d running environment(s)\n%!" (List.length envs);
  List.iter
    (fun (env : Ona.env) ->
      let checkout = Option.value env.checkout_location ~default:"workspace" in
      Printf.printf "  - %s [%s] (%s)\n%!" env.id env.nickname checkout)
    envs;
  match update_config config_path envs with
  | Some (ona_count, non_ona_count) ->
      Printf.printf "Updated Zed config (%d Ona, %d non-Ona connections)\n%!"
        ona_count non_ona_count
  | None ->
      Printf.printf "Failed to update Zed config\n%!"

(** Main entry point *)
let run config_path interval once =
  Printf.printf "ona-sync-zed-ssh-connections starting...\n%!";
  Printf.printf "Config path: %s\n%!" config_path;
  if once then
    sync config_path
  else begin
    while true do
      sync config_path;
      Printf.printf "Sleeping for %d seconds...\n%!" interval;
      Unix.sleep interval
    done
  end

(* Command line interface *)
let config_arg =
  let doc = "Path to Zed settings.json file." in
  Arg.(value & opt string default_zed_config_path & info ["c"; "config"] ~docv:"PATH" ~doc)

let interval_arg =
  let doc = "Sync interval in seconds." in
  Arg.(value & opt int 30 & info ["i"; "interval"] ~docv:"SECONDS" ~doc)

let once_arg =
  let doc = "Run once and exit instead of continuously syncing." in
  Arg.(value & flag & info ["once"] ~doc)

let cmd =
  let doc = "Sync Ona environments to Zed's ssh_connections config" in
  let man = [
    `S Manpage.s_description;
    `P "Watches for running Ona environments and keeps Zed's ssh_connections \
        configuration in sync. Non-Ona SSH connections are preserved.";
    `S Manpage.s_examples;
    `P "Run continuously with default settings:";
    `Pre "  ona-sync-zed-ssh-connections";
    `P "Sync once and exit:";
    `Pre "  ona-sync-zed-ssh-connections --once";
    `P "Sync every 60 seconds:";
    `Pre "  ona-sync-zed-ssh-connections -i 60";
  ] in
  let info = Cmd.info "ona-sync-zed-ssh-connections" ~version:"1.0.0" ~doc ~man in
  Cmd.v info Term.(const run $ config_arg $ interval_arg $ once_arg)

let () = exit (Cmd.eval cmd)
