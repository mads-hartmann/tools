open Cmdliner

let months = ["Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"]

let is_digit c = c >= '0' && c <= '9'

(* Timestamp is always first 15 chars: "Mon DD HH:MM:SS" *)
let extract_timestamp line =
  if String.length line < 15 then None
  else
    let ts = String.sub line 0 15 in
    let month = String.sub ts 0 3 in
    if List.mem month months
       && ts.[3] = ' '
       && is_digit ts.[4] && is_digit ts.[5]
       && ts.[6] = ' '
       && is_digit ts.[7] && is_digit ts.[8] && ts.[9] = ':'
       && is_digit ts.[10] && is_digit ts.[11] && ts.[12] = ':'
       && is_digit ts.[13] && is_digit ts.[14]
    then Some ts
    else None

let extract_json_msg line =
  let msg_re = Str.regexp "\"msg\"[ \t]*:[ \t]*\"\\([^\"]+\\)\"" in
  try
    let _ = Str.search_forward msg_re line 0 in
    Some (Str.matched_group 1 line)
  with Not_found -> None

let extract_info_msg line =
  let info_re = Str.regexp "\\[id:[^]]+\\] \\[[^]]+\\] INFO \\(.*\\)" in
  try
    let _ = Str.search_forward info_re line 0 in
    Some (Str.matched_group 1 line)
  with Not_found -> None

let process_line line =
  match extract_timestamp line with
  | None -> None
  | Some timestamp ->
      let msg = match extract_json_msg line with
        | Some m -> Some m
        | None -> extract_info_msg line
      in
      Option.map (Printf.sprintf "%s | %s" timestamp) msg

let process_channel ic =
  try
    while true do
      let line = input_line ic in
      match process_line line with
      | Some output -> print_endline output
      | None -> ()
    done
  with End_of_file -> ()

let run file =
  match file with
  | Some path ->
      let ic = open_in path in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () -> process_channel ic)
  | None ->
      process_channel stdin

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
