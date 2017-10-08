open Core

type command =
  string array
[@@deriving sexp]

type watch_action =
  | Ignore
  | Run of command
  | Timeout of (float * command)
  | Seq of watch_action list
[@@deriving sexp]

type watch_config =
  (string * watch_action) list
[@@deriving sexp]

let to_watch_config config =
  List.map config ~f:(fun (cmd, action) ->
      let rx = Re.compile (Re_glob.glob cmd) in
      (rx, action)
    )

let pp_command (_, args) =
  Printf.sprintf "%s" (String.concat_array ~sep:" " args)

let watch_files dir callback =
  let flags = Fsevents.CreateFlags.detailed_interactive in
  let watcher = Fsevents_lwt.create 0. flags [dir] in
  let stream = Fsevents_lwt.stream watcher in
  let event_stream = Fsevents_lwt.event_stream watcher in
  Lwt.async (fun () -> Lwt_stream.iter_s callback stream);
  Lwt.async (fun () -> Cf_lwt.RunLoop.run_thread (fun runloop ->
      Fsevents.schedule_with_run_loop event_stream runloop Cf.RunLoop.Mode.Default;
      if not (Fsevents.start event_stream) then
        prerr_endline "failed to start FSEvents stream"
    ));
  let promise, resolver = Lwt.wait () in
  Lwt_main.run promise

let to_lwt_cmd cmd =
  ("", cmd)

let rec invoke_action action =
  let run_command ?timeout cmd =
    let cmd = to_lwt_cmd cmd in
    Lwt.(
      Lwt_io.printf "running %s\n" (pp_command cmd)
      >>= (fun _ ->
          match timeout with
          | Some ms -> Lwt_process.exec ~timeout:ms cmd
          | None -> Lwt_process.exec cmd
        )
      >|= ignore
    )
  in
  match action with
  | Ignore ->
    Lwt.return_unit
  | Run cmd ->
    run_command cmd
  | Timeout (ms, cmd) ->
    run_command ~timeout:ms cmd
  | Seq actions ->
    Lwt_list.fold_left_s (fun () -> invoke_action) () actions

let read_config file : watch_config =
  let config =
    try
      In_channel.read_all file |>
      Sexp.of_string |>
      watch_config_of_sexp
    with
      Sexplib.Conv.Of_sexp_error (e, sexp) ->
      print_endline "failed to parse configuration file. continuing with empty config";
      print_endline (Exn.to_string e);
      []
  in
  print_endline "current configuration:";
  print_endline (Sexp.to_string_hum (sexp_of_watch_config config));
  config

let run dir ~config_file =
  let config = ref (read_config config_file) in
  let find_action path =
    let rec loop = function
      | [] ->
        None
      | (rx, action) :: rest ->
        if Re.execp rx path then
          Some action
        else
          loop rest
    in loop (to_watch_config !config)
  in
  let () = Sys.chdir dir in
  let cwd = Sys.getcwd () in
  let len = String.length cwd in
  print_endline ("current directory is " ^ cwd);
  ignore (watch_files cwd (fun {path} ->
      let p = String.drop_prefix path (len + 1) in
      print_endline (p ^ " changed");
      if p = config_file then (
        print_endline "configuration changed";
        config := read_config config_file;
        Lwt.return_unit
      ) else
        Option.value ~default:Lwt.return_unit
          (Option.map
             ~f:invoke_action
             (find_action p))
    ))
