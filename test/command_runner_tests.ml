open Core
open Butler

type 'a timeout =
  [ `Ok of 'a | `Timeout ]
[@@deriving sexp]

let run_with_timeout t cmd = Lwt.(
    catch
      (fun () -> pick [Lwt_unix.timeout t; cmd] >|= fun res -> `Ok res)
      (function
        | Lwt_unix.Timeout -> Lwt.return `Timeout
        | exn -> raise exn))

let equal_sexp =
  let fmt_action fmt sexp =
    Fmt.pf fmt "%s" (Sexplib__Sexp.to_string_hum sexp)
  in
  Alcotest.testable fmt_action Sexp.equal

let test_run_with_timeout_times_out () =
  Alcotest.(check equal_sexp) "times out"
    [%sexp (`Timeout)]
    (sexp_of_timeout
      sexp_of_unit
      (Lwt_main.run (run_with_timeout 0.1 (Lwt_unix.sleep 1.0))))

let test_run_with_timeout_succeeds () =
  Alcotest.(check equal_sexp) "finishes before timeout"
    [%sexp (`Ok ())]
    (sexp_of_timeout
      sexp_of_unit
      (Lwt_main.run (run_with_timeout 1.0 (Lwt_unix.sleep 0.1))))

let test_filter_matching_actions () =
  let config = to_watch_config (
      [ Glob "*.js", Ignore
      ; Glob "main.js", Run [|"some"; "command"|]
      ; Glob "hello.js", Timeout (1.0, [|"node"|])
      ])
  in
  Alcotest.(check equal_sexp) "action list"
    [%sexp ([Ignore; Run [|"some"; "command"|]] : watch_action list)]
    (sexp_of_list sexp_of_watch_action (find_actions config "main.js"))

(* @TODO: merge with butler.ml *)

let to_lwt_cmd cmd =
  ("", cmd)

type 'a watch_action =
  | Ignore
  | Run of 'a
  | Timeout of (float * 'a)
[@@deriving sexp]

let to_lwt_processes actions =
  List.map actions ~f:(function
      | Ignore ->
        `Ignore
      | Run cmd ->
        `Run (fun () -> Lwt_process.exec (to_lwt_cmd cmd))
      | Timeout (timeout, cmd) ->
        `Run (fun () -> Lwt_process.exec ~timeout (to_lwt_cmd cmd))
    )

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

let test_action_order () =
  let ok f = f (); Lwt.return (Lwt_unix.WEXITED 0) in
  let output = ref [] in
  let actions =
    [ `Run (fun () -> ok (fun () -> output := !output @ ["a"]))
    ; `Run (fun () -> ok (fun () -> output := !output @ ["b"]))
    ; `Run (fun () -> ok (fun () -> output := !output @ ["c"]))
    ]
  in
  let _ = Lwt_main.run (execute_actions actions) in
  Alcotest.(check (list string)) "in order" ["a"; "b"; "c"] !output

let test_ignore () =
  let ok f = f (); Lwt.return (Lwt_unix.WEXITED 0) in
  let output = ref [] in
  let actions =
    [ `Run (fun () -> ok (fun () -> output := !output @ ["a"]))
    ; `Ignore
    ; `Run (fun () -> ok (fun () -> output := !output @ ["b"]))
    ]
  in
  let _ = Lwt_main.run (execute_actions actions) in
  Alcotest.(check (list string)) "is ignore" ["a"] !output

let tests =
  [ "times out", `Slow, test_run_with_timeout_times_out
  ; "finishes in time", `Slow, test_run_with_timeout_succeeds
  ; "filter matching actions", `Quick, test_filter_matching_actions
  ; "action are executed in order", `Quick, test_action_order
  ; "ignore stops execution", `Quick, test_ignore
  ]
