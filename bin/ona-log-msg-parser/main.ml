(** ona-log-msg-parser: Extract timestamp and msg from ona-swe-agent-service.log

    Handles both JSON log lines (with "msg" field) and plain INFO lines.
    The log format is systemd journal style with optional embedded JSON. *)

open Cmdliner

(** Extract timestamp from beginning of line (e.g., "Dec 05 10:35:00") *)
let extract_timestamp line =
  let month_pattern = "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)" in
  let timestamp_re = Str.regexp (month_pattern ^ " [0-9][0-9] [0-9][0-9]:[0-9][0-9]:[0-9][0-9]") in
  if Str.string_match timestamp_re line 0 then
    Some (Str.matched_string line)
  else
    None

(** Extract "msg" value from JSON in line *)
let extract_json_msg line =
  let msg_re = Str.regexp "\"msg\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" in
  try
    let _ = Str.search_forward msg_re line 0 in
    Some (Str.matched_group 1 line)
  with Not_found -> None

(** Extract INFO message from non-JSON lines
    Pattern: [id:...] [...] INFO <message> *)
let extract_info_msg line =
  let info_re = Str.regexp "\\[id:[^]]+\\] \\[[^]]+\\] INFO \\(.*\\)" in
  try
    let _ = Str.search_forward info_re line 0 in
    Some (Str.matched_group 1 line)
  with Not_found -> None

(** Process a single line and return formatted output if valid *)
let process_line line =
  match extract_timestamp line with
  | None -> None
  | Some timestamp ->
      (* Try JSON msg first, then INFO pattern *)
      let msg =
        match extract_json_msg line with
        | Some m -> Some m
        | None -> extract_info_msg line
      in
      match msg with
      | Some m -> Some (Printf.sprintf "%s | %s" timestamp m)
      | None -> None

(** Process input channel line by line *)
let process_channel ic =
  try
    while true do
      let line = input_line ic in
      match process_line line with
      | Some output -> print_endline output
      | None -> ()
    done
  with End_of_file -> ()

(** Main entry point *)
let run file =
  match file with
  | Some path ->
      let ic = open_in path in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () -> process_channel ic)
  | None ->
      process_channel stdin

(* Command line interface *)
let file_arg =
  let doc = "Log file to parse. Reads from stdin if not provided." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Extract timestamp and msg from ona-swe-agent-service logs" in
  let man = [
    `S Manpage.s_description;
    `P "Parses log files in systemd journal format with embedded JSON. \
        Extracts timestamps and message content from both JSON log lines \
        (with \"msg\" field) and plain INFO lines.";
    `S Manpage.s_examples;
    `P "Parse a log file:";
    `Pre "  ona-log-msg-parser ona-swe-agent-service.log";
    `P "Parse from stdin:";
    `Pre "  cat ona-swe-agent-service.log | ona-log-msg-parser";
  ] in
  let info = Cmd.info "ona-log-msg-parser" ~version:"1.0.0" ~doc ~man in
  Cmd.v info Term.(const run $ file_arg)

let () = exit (Cmd.eval cmd)
