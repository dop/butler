open Core
open Cohttp_lwt_unix

type command =
  string array
[@@deriving sexp]

type watch_action =
  | Ignore
  | Run of command
  | Timeout of (float * command)
[@@deriving sexp]

type 'a watch_matcher =
  | Glob of 'a
  | And of 'a watch_matcher list
  | Or of 'a watch_matcher list
[@@deriving sexp]

type watch_config =
  (string watch_matcher * watch_action) list
[@@deriving sexp]

let to_watch_config config =
  let to_rx str = Re.compile (Re_glob.glob ~expand_braces:true str) in
  let rec convert = function
    | Glob str ->
      Glob (to_rx str)
    | And xs ->
      And (List.map ~f:convert xs)
    | Or xs ->
      Or (List.map ~f:convert xs)
  in
  List.map config ~f:(fun (matcher, action) ->
      (convert matcher, action)
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
  let promise, _ = Lwt.wait () in
  Lwt_main.run promise

let to_lwt_cmd cmd =
  ("", cmd)

let rec invoke_action file actions =
  let run_command ?timeout cmd =
    let cmd = to_lwt_cmd cmd in
    Lwt.(
      Lwt_io.printf ">>> %s, triggered by %s\n" (pp_command cmd) file
      >>= (fun _ ->
          match timeout with
          | Some ms -> Lwt_process.exec ~timeout:ms cmd
          | None -> Lwt_process.exec cmd
        )
    )
  in
  let ifOk action next =
    Lwt.(action >>= function
      | UnixLabels.WEXITED 0 -> next
      | status -> Lwt.return status
      )
  in
  let ok = UnixLabels.WEXITED 0 in
  match actions with
  | [] ->
    Lwt.return ok
  | Ignore :: _ ->
    Lwt.return ok
  | Run cmd :: actions ->
    ifOk (run_command cmd) (invoke_action file actions)
  | Timeout (ms, cmd) :: actions ->
    ifOk (run_command ~timeout:ms cmd) (invoke_action file actions)

let invoke file action =
  Lwt.(invoke_action file action >|= ignore)

let read_config file : watch_config =
  let config =
    try
      In_channel.read_all file |>
      Sexp.of_string |>
      watch_config_of_sexp
    with
      Sexplib.Conv.Of_sexp_error (e, _) ->
      print_endline "failed to parse configuration file. continuing with empty config";
      print_endline (Exn.to_string e);
      []
  in
  print_endline "current configuration:";
  print_endline (Sexp.to_string_hum (sexp_of_watch_config config));
  config

let rec matches path = function
  | Glob rx ->
    Re.execp rx path
  | And xs ->
    List.for_all ~f:(matches path) xs
  | Or xs ->
    List.exists ~f:(matches path) xs

let make_file_server root =
  let ok body =
    Server.respond_string ~status:`OK ~body ()
  in
  let serve_file fname =
    Server.respond_file ~fname ()
  in
  let not_found () =
    Server.respond_string ~status:`Not_found ~body:"Not found" ()
  in
  print_endline ("serving " ^ root);
  let len = String.length root in
  let drop_root filepath =
    String.drop_prefix filepath len
  in
  fun req _ ->
    let uri_path = String.drop_prefix (Uri.path (Request.uri req)) 1 in
    let files = FileUtil.ls root in
    if String.length uri_path > 0 then
      match List.find ~f:(String.is_suffix ~suffix:uri_path) files with
      | Some filepath ->
        serve_file filepath
      | None ->
        not_found ()
    else
      ok (Sexp.to_string (sexp_of_list sexp_of_string (List.map ~f:drop_root files)))

let run_server port handler =
  let server = Server.make ~callback:(fun _ -> handler) () in
  Server.create ~mode:(`TCP (`Port port)) server

let run ?(serve=false) ~config_file dir =
  let config = ref (read_config config_file) in
  let find_actions path =
    let rec loop actions = function
      | [] ->
        List.rev actions
      | (matcher, action) :: rest ->
        if matches path matcher then
          loop (action :: actions) rest
        else
          loop actions rest
    in loop [] (to_watch_config !config)
  in
  let () = Sys.chdir dir in
  let cwd = Sys.getcwd () in
  let len = String.length cwd in
  print_endline ("current directory is " ^ cwd);
  if serve then
    Lwt.async (fun () ->
        print_endline ("serving " ^ cwd ^ " on port 8080");
        run_server 8080 (make_file_server cwd)
      );
  ignore (watch_files cwd (fun {path; _} ->
      let p = String.drop_prefix path (len + 1) in
      (* print_endline (p ^ " changed"); *)
      if p = config_file then (
        print_endline "configuration changed";
        config := read_config config_file;
        Lwt.return_unit
      ) else
        invoke p (find_actions p)
    ))
