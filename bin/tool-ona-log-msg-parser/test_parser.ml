let months = ["Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"]

let is_digit c = c >= '0' && c <= '9'

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

(* Test helpers *)
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

let assert_eq expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "expected %S, got %S"
      (match expected with Some s -> s | None -> "None")
      (match actual with Some s -> s | None -> "None"))



(* Test cases from ona-swe-agent-service.log *)
let () =
  print_endline "Testing extract_timestamp:";

  test "valid timestamp" (fun () ->
    assert_eq (Some "Dec 05 10:35:00") (extract_timestamp "Dec 05 10:35:00 ip-172-31-28-113 systemd[1]: Starting..."));

  test "all months work" (fun () ->
    List.iter (fun m ->
      let line = Printf.sprintf "%s 01 00:00:00 test" m in
      assert_eq (Some (Printf.sprintf "%s 01 00:00:00" m)) (extract_timestamp line)
    ) months);

  test "line too short" (fun () ->
    assert_eq None (extract_timestamp "Dec 05 10:35"));

  test "invalid month" (fun () ->
    assert_eq None (extract_timestamp "Xyz 05 10:35:00 test"));

  test "invalid format" (fun () ->
    assert_eq None (extract_timestamp "2025-12-05T10:35:00Z test"));

  print_endline "\nTesting extract_json_msg:";

  test "extracts msg from JSON" (fun () ->
    let line = {|Dec 05 10:35:04 ip-172-31-28-113 supervisor[9612]: [id:running-service] [2025-12-05T10:35:04Z] INFO {"time":"2025-12-05T10:35:04.761422157Z","level":"INFO","msg":"Starting ONA SWE agent with conversation manager","version":"main-gha.18859"}|} in
    assert_eq (Some "Starting ONA SWE agent with conversation manager") (extract_json_msg line));

  test "extracts msg with spaces around colon" (fun () ->
    let line = {|{"msg" : "test message"}|} in
    assert_eq (Some "test message") (extract_json_msg line));

  test "no msg field" (fun () ->
    let line = {|{"time":"2025-12-05","level":"INFO"}|} in
    assert_eq None (extract_json_msg line));

  print_endline "\nTesting extract_info_msg:";

  test "extracts INFO message" (fun () ->
    let line = "Dec 05 10:35:00 ip-172-31-28-113 supervisor[9612]: [id:running-service] [2025-12-05T10:35:00Z] INFO Starting service" in
    assert_eq (Some "Starting service") (extract_info_msg line));

  test "extracts INFO with exitCode" (fun () ->
    let line = "Dec 05 10:35:01 ip-172-31-28-113 supervisor[9612]: [id:ready-command] [2025-12-05T10:35:01Z] INFO Service is not ready exitCode=7" in
    assert_eq (Some "Service is not ready exitCode=7") (extract_info_msg line));

  test "no INFO pattern" (fun () ->
    let line = "Dec 05 10:35:00 ip-172-31-28-113 systemd[1]: Starting automations-service..." in
    assert_eq None (extract_info_msg line));

  print_endline "\nTesting process_line (full pipeline):";

  test "JSON log line" (fun () ->
    let line = {|Dec 05 10:35:05 ip-172-31-28-113 supervisor[9612]: [id:running-service] [2025-12-05T10:35:05Z] INFO {"time":"2025-12-05T10:35:05.040862249Z","level":"INFO","msg":"Feature flags initialized successfully"}|} in
    assert_eq (Some "Dec 05 10:35:05 | Feature flags initialized successfully") (process_line line));

  test "plain INFO line" (fun () ->
    let line = "Dec 05 10:35:06 ip-172-31-28-113 supervisor[9612]: [id:ready-command] [2025-12-05T10:35:06Z] INFO Service is ready" in
    assert_eq (Some "Dec 05 10:35:06 | Service is ready") (process_line line));

  test "systemd line without msg (skipped)" (fun () ->
    let line = "Dec 05 10:35:00 ip-172-31-28-113 systemd[1]: Starting automations-service-agent-00000000-0000-0000-0000-000000007100.service" in
    assert_eq None (process_line line));

  test "section-create line without msg (skipped)" (fun () ->
    let line = {|Dec 05 10:35:00 ip-172-31-28-113 supervisor[9612]: [section-create] {"id":"running-service","title":"Running service"}|} in
    assert_eq None (process_line line));

  test "URL in INFO message" (fun () ->
    let line = "Dec 05 10:35:04 ip-172-31-28-113 supervisor[9612]: [id:running-service] [2025-12-05T10:35:04Z] INFO https://61000--019aee13-0060-7b98-b056-2ff1e371167f.eu-runner.flex.doptig.cloud" in
    assert_eq (Some "Dec 05 10:35:04 | https://61000--019aee13-0060-7b98-b056-2ff1e371167f.eu-runner.flex.doptig.cloud") (process_line line));

  (* Summary *)
  print_endline "";
  Printf.printf "Results: %d/%d tests passed\n" !tests_passed !tests_run;
  if !tests_passed <> !tests_run then exit 1
