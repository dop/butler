open Core
open Butler_utils

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

let run ?(serve=false) ~config_file dir =
  let config = ref (to_watch_config (read_config config_file)) in
  let () = Sys.chdir dir in
  let cwd = Sys.getcwd () in
  let len = String.length cwd in
  if serve then
    Lwt.async (fun () ->
        print_endline ("serving " ^ cwd ^ " on port 8080");
        Butler_server.run 8080 (Butler_server.make_file_server cwd)
      );
  Lwt_main.run (watch_files cwd (fun _ path ->
      let p = String.drop_prefix path (len + 1) in
      if p = config_file then (
        config := to_watch_config (read_config config_file);
        Lwt.return_unit
      ) else (
        Lwt.(execute_actions (find_actions !config p) >|= ignore)
      )
    ))
