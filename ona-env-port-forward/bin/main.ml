(** ona-env-port-forward: Port forwarding for Ona environments with auto-reconnect *)

(* ============================================================================
   Types
   ============================================================================ *)

type connection_record = {
  start_time : float;
  end_time : float option;
}

type session_stats = {
  mutable connection_count : int;
  mutable connections : connection_record list;
}

(* ============================================================================
   Utilities
   ============================================================================ *)

(** Format duration in human-readable form *)
let format_duration seconds =
  let seconds = int_of_float seconds in
  let hours = seconds / 3600 in
  let minutes = (seconds mod 3600) / 60 in
  let secs = seconds mod 60 in
  if hours > 0 then Printf.sprintf "%dh%dm%ds" hours minutes secs
  else if minutes > 0 then Printf.sprintf "%dm%ds" minutes secs
  else Printf.sprintf "%ds" secs

(** Get current timestamp as formatted string *)
let timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(** Log a message with timestamp *)
let log msg =
  Printf.printf "[%s] %s\n%!" (timestamp ()) msg

(* ============================================================================
   Interactive selection using MintTea
   ============================================================================ *)

module Selection_tui = struct
  open Minttea

  type model = {
    environments : Ona.env list;
    cursor : int;
    selected : Ona.env option;
  }

  let initial_model environments =
    {
      environments;
      cursor = 0;
      selected = None;
    }

  let init _model = Command.Noop

  let update event model =
    match event with
    | Event.KeyDown (Key "q" | Escape) ->
        (model, Command.Quit)
    | Event.KeyDown Enter ->
        let selected =
          if model.cursor < List.length model.environments then
            Some (List.nth model.environments model.cursor)
          else
            None
        in
        ({ model with selected }, Command.Quit)
    | Event.KeyDown (Up | Key "k") ->
        let cursor =
          if model.cursor > 0 then model.cursor - 1
          else List.length model.environments - 1
        in
        ({ model with cursor }, Command.Noop)
    | Event.KeyDown (Down | Key "j") ->
        let cursor =
          if model.cursor < List.length model.environments - 1 then model.cursor + 1
          else 0
        in
        ({ model with cursor }, Command.Noop)
    | _ -> (model, Command.Noop)

  let view model =
    let open Spices in
    let header = "Select an Ona environment:\n\n" in
    let items =
      List.mapi
        (fun i (env : Ona.env) ->
          let cursor_char = if i = model.cursor then "▸ " else "  " in
          let styled =
            if i = model.cursor then
              default |> fg (color "#00ff00") |> bold true |> build
            else
              default |> fg (color "#888888") |> build
          in
          cursor_char ^ styled "%s (%s)" env.nickname env.id)
        model.environments
    in
    let footer = "\n\n↑/k up • ↓/j down • enter select • q quit" in
    header ^ String.concat "\n" items ^ footer

  let run environments =
    let initial = initial_model environments in
    (* Use a mutable ref to capture the final model state *)
    let final_model = ref initial in
    let wrapped_update event model =
      let new_model, cmd = update event model in
      final_model := new_model;
      (new_model, cmd)
    in
    let app = Minttea.app ~init ~update:wrapped_update ~view () in
    Minttea.start app ~initial_model:initial;
    (!final_model).selected
end

(* ============================================================================
   SSH Port Forwarding with Auto-Reconnect
   ============================================================================ *)

let stats = {
  connection_count = 0;
  connections = [];
}

let current_connection : connection_record option ref = ref None

(** Print session summary *)
let print_summary () =
  let total_uptime =
    List.fold_left
      (fun acc conn ->
        let end_t = Option.value conn.end_time ~default:(Unix.time ()) in
        acc +. (end_t -. conn.start_time))
      0.0 stats.connections
  in
  let avg_duration =
    if stats.connection_count > 0 then total_uptime /. float_of_int stats.connection_count
    else 0.0
  in
  Printf.printf "\nSession summary: %d connection(s), total uptime %s, avg connection %s\n%!"
    stats.connection_count
    (format_duration total_uptime)
    (format_duration avg_duration)

(** Handle SIGINT to print summary before exit *)
let setup_signal_handler () =
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ ->
         (* End current connection if any *)
         (match !current_connection with
          | Some conn ->
              let ended = { conn with end_time = Some (Unix.time ()) } in
              stats.connections <- ended :: (List.tl stats.connections)
          | None -> ());
         print_summary ();
         exit 0))

(** Run SSH port forwarding with auto-reconnect *)
let run_port_forward (env : Ona.env) port =
  let host = Printf.sprintf "%s.gitpod.environment" env.id in
  let port_str = string_of_int port in
  let local_forward = Printf.sprintf "0.0.0.0:%s:localhost:%s" port_str port_str in

  setup_signal_handler ();

  log (Printf.sprintf "Starting port forwarding to %s (port %d)" env.nickname port);

  let rec loop () =
    stats.connection_count <- stats.connection_count + 1;
    let conn = { start_time = Unix.time (); end_time = None } in
    stats.connections <- conn :: stats.connections;
    current_connection := Some conn;

    log (Printf.sprintf "Connected to %s (port %d)" env.nickname port);

    (* Fork and exec SSH *)
    let pid =
      Unix.create_process "ssh"
        [| "ssh"; "-N"; "-o"; "GatewayPorts=yes"; "-o"; "ServerAliveInterval=30";
           "-o"; "ServerAliveCountMax=3"; "-L"; local_forward; host |]
        Unix.stdin Unix.stdout Unix.stderr
    in

    (* Wait for SSH to exit *)
    let _, status = Unix.waitpid [] pid in
    let end_time = Unix.time () in

    (* Update connection record *)
    let conn = List.hd stats.connections in
    let ended = { conn with end_time = Some end_time } in
    stats.connections <- ended :: (List.tl stats.connections);
    current_connection := None;

    let duration = end_time -. conn.start_time in

    let exit_info =
      match status with
      | Unix.WEXITED code -> Printf.sprintf "exited with code %d" code
      | Unix.WSIGNALED sig_num -> Printf.sprintf "killed by signal %d" sig_num
      | Unix.WSTOPPED sig_num -> Printf.sprintf "stopped by signal %d" sig_num
    in

    log (Printf.sprintf "Disconnected after %s (%s, reconnecting... attempt #%d)"
           (format_duration duration) exit_info (stats.connection_count + 1));

    (* Brief delay before reconnecting *)
    Unix.sleep 2;
    loop ()
  in
  loop ()

(* ============================================================================
   Main entry point
   ============================================================================ *)

let usage () =
  Printf.printf "Usage: ona-env-port-forward [port]\n\n";
  Printf.printf "Port forwards to a selected Ona environment with auto-reconnect.\n\n";
  Printf.printf "Arguments:\n";
  Printf.printf "  port    Local port to forward (default: 5173)\n\n";
  Printf.printf "Options:\n";
  Printf.printf "  -h, --help    Show this help message\n%!";
  exit 0

let () =
  (* Parse command line arguments *)
  let port =
    if Array.length Sys.argv > 1 then
      let arg = Sys.argv.(1) in
      if arg = "-h" || arg = "--help" then usage ()
      else
        try int_of_string arg
        with _ ->
          Printf.eprintf "Invalid port: %s\n%!" arg;
          Printf.eprintf "Use --help for usage information.\n%!";
          exit 1
    else 5173
  in

  (* List environments *)
  let envs = Ona.list_environments () in

  if List.length envs = 0 then (
    Printf.printf "No running Ona environments found.\n%!";
    exit 0
  );

  (* Show selection using MintTea TUI *)
  match Selection_tui.run envs with
  | None ->
      Printf.printf "No environment selected.\n%!";
      exit 0
  | Some env ->
      run_port_forward env port
