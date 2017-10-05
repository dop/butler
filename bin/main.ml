(* TODO
   - Move stuff to lib
*)

open Core

type watch_action =
  | Ignore
  | Run of string array
[@@deriving sexp]

type watch_config =
  (string * watch_action) list
[@@deriving sexp]

let my_conf_ =
  [ "_build", Ignore
  ; "bin/main.ml", Run [|"jbuilder"; "build"; "bin/main.exe"|]
  ]

let my_conf =
  my_conf_ |> List.map ~f:(fun (cmd, action) ->
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

let () =
  let cwd = Sys.getcwd () in
  let len = String.length cwd in
  Lwt.async (fun () -> Lwt_io.printf "current directory is \"%s\"\n" cwd);
  Lwt.async (fun () -> Lwt_io.printf "%s\n" (Sexp.to_string_hum (sexp_of_watch_config my_conf_)));
  watch_files cwd (fun {path} ->
      with_return (fun {return} ->
          let p = String.drop_prefix path (len + 1) in
          let rec loop = function
            | [] ->
              Lwt.return_unit
            | (rx, action) :: rest ->
              if Re.execp rx p then
                match action with
                | Ignore ->
                  Lwt.return_unit
                | Run cmd ->
                  let cmd = ("", cmd) in
                  Lwt.(
                    Lwt_io.printf "%s changed, running %s\n" p (pp_command cmd)
                    >>= fun _ -> Lwt_process.exec cmd
                    >|= ignore
                  )
              else
                loop rest
          in
          return (loop my_conf)
        )
    )
