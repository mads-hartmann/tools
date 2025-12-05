(** Ona: Shared library for Ona environment tools *)

(** Path to the Ona CLI (still named gitpod for now) *)
let cli_path = "/usr/local/bin/gitpod"

(** Represents an Ona environment *)
type env = {
  id : string;
  nickname : string;
  checkout_location : string option;
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

(** Parse environment list JSON *)
let parse_envs ?(include_checkout_location = false) json_str =
  try
    let json = Yojson.Basic.from_string json_str in
    let open Yojson.Basic.Util in
    json
    |> to_list
    |> List.map (fun env_json ->
           {
             id = env_json |> member "id" |> to_string;
             nickname = extract_nickname env_json;
             checkout_location =
               if include_checkout_location then extract_checkout_location env_json
               else None;
           })
  with _ -> []

(** List running Ona environments *)
let list_environments ?(include_checkout_location = false) () =
  let output =
    run_command cli_path [ "environment"; "list"; "-o"; "json"; "--running-only"; "2>/dev/null" ]
  in
  parse_envs ~include_checkout_location output
