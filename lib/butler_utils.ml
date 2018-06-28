open Core

type command =
  string array
[@@deriving sexp]

type 'a watch_action =
  | Ignore
  | Run of 'a
  | Timeout of (float * 'a)
[@@deriving sexp]

type 'a watch_matcher =
  | Glob of 'a
  | And of 'a watch_matcher list
  | Or of 'a watch_matcher list
[@@deriving sexp]

type watch_config =
  (string watch_matcher * command watch_action) list
[@@deriving sexp]

type executable =
  [ `Run of unit -> Lwt_unix.process_status Lwt.t
  | `Ignore
  ]
[@@deriving sexp]

type 'a timeout =
  [ `Ok of 'a | `Timeout ]
[@@deriving sexp]

let run_with_timeout t f = Lwt.(
    catch
      (fun () -> pick [Lwt_unix.timeout t; f ()] >|= fun res -> `Ok res)
      (function
        | Lwt_unix.Timeout -> Lwt.return `Timeout
        | exn -> raise exn))

let to_lwt_cmd cmd =
  ("", cmd)

let cmd_to_lwt = function
  | Ignore ->
    `Ignore
  | Run cmd ->
    `Run (fun () -> Lwt_process.exec (to_lwt_cmd cmd))
  | Timeout (timeout, cmd) ->
    `Run (fun () -> Lwt_process.exec ~timeout (to_lwt_cmd cmd))

let to_glob matcher =
  let to_rx str = Re.compile (Re.Glob.glob ~expand_braces:true str) in
  let rec convert = function
    | Glob str ->
      Glob (to_rx str)
    | And xs ->
      And (List.map ~f:convert xs)
    | Or xs ->
      Or (List.map ~f:convert xs)
  in
  convert matcher

let to_watch_config config =
  let to_rx str = Re.compile (Re.Glob.glob ~expand_braces:true str) in
  let rec convert = function
    | Glob str ->
      Glob (to_rx str)
    | And xs ->
      And (List.map ~f:convert xs)
    | Or xs ->
      Or (List.map ~f:convert xs)
  in
  List.map config ~f:(fun (matcher, action) ->
      (convert matcher, cmd_to_lwt action)
    )

let find_actions config path =
  let rec matches path = function
  | Glob rx ->
    Re.execp rx path
  | And xs ->
    List.for_all ~f:(matches path) xs
  | Or xs ->
    List.exists ~f:(matches path) xs
  in
  List.filter ~f:(fun (m, _) -> matches path m) config
  |> List.map ~f:snd

let execute_actions actions =
  let ok = Lwt_unix.WEXITED 0 in
  let ( >> ) action next =
    Lwt.(action >>= function
      | UnixLabels.WEXITED 0 -> next ()
      | status -> Lwt.return status
      )
  in
  let rec loop = function
    | [] ->
      Lwt.return ok
    | `Ignore :: _ ->
      Lwt.return ok
    | `Run action :: rest ->
      action () >> (fun () -> loop rest)
  in
  loop actions

let watch_files dir callback =
  let flags = Fsevents.CreateFlags.detailed_interactive in
  let watcher = Fsevents_lwt.create 0. flags [dir] in
  let stream = Fsevents_lwt.stream watcher in
  let event_stream = Fsevents_lwt.event_stream watcher in
  let promise, waker = Lwt.task () in
  Lwt.async (fun () ->
      Lwt_stream.iter_s (fun {Fsevents_lwt.path; _} -> callback waker path) stream
    );
  Lwt.async (fun () -> Cf_lwt.RunLoop.run_thread (fun runloop ->
      Fsevents.schedule_with_run_loop event_stream runloop Cf.RunLoop.Mode.Default;
      if not (Fsevents.start event_stream) then
        prerr_endline "failed to start FSEvents stream"
    ));
  Lwt.on_termination promise (fun () ->
      Fsevents_lwt.stop watcher;
      Fsevents_lwt.release watcher;
    );
  promise
