open Core
open Butler_utils

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
      (Lwt_main.run (run_with_timeout 0.1 (fun () -> Lwt_unix.sleep 1.0))))

let test_run_with_timeout_succeeds () =
  Alcotest.(check equal_sexp) "finishes before timeout"
    [%sexp (`Ok ())]
    (sexp_of_timeout
      sexp_of_unit
      (Lwt_main.run (run_with_timeout 1.0 (fun () -> Lwt_unix.sleep 0.1))))

let ok f =
  f ();
  Lwt.return (Lwt_unix.WEXITED 0)

let append r x =
  r := !r @ [x]

let test_action_order () =
  let output = ref [] in
  let actions =
    [ `Run (fun () -> ok (fun () -> append output "a"))
    ; `Run (fun () -> ok (fun () -> append output "b"))
    ; `Run (fun () -> ok (fun () -> append output "c"))
    ]
  in
  let _ = Lwt_main.run (execute_actions actions) in
  Alcotest.(check (list string)) "in order" ["a"; "b"; "c"] !output

let test_ignore () =
  let output = ref [] in
  let actions =
    [ `Run (fun () -> ok (fun () -> append output "a"))
    ; `Ignore
    ; `Run (fun () -> ok (fun () -> append output "b"))
    ]
  in
  let _ = Lwt_main.run (execute_actions actions) in
  Alcotest.(check (list string)) "is ignore" ["a"] !output

let test_actions_filtering () =
  let output = ref [] in
  let config = (
    [ to_glob (Glob "*.js"), `Run (fun () -> ok (fun () -> append output "a"))
    ; to_glob (Glob "main.js"), `Run (fun () -> ok (fun () -> append output "b"))
    ; to_glob (Glob "hello.js"), `Run (fun () -> ok (fun () -> append output "c"))
    ]
    )
  in
  let _ = Lwt_main.run (find_actions config "main.js" |> execute_actions) in
  Alcotest.(check (list string)) "in order and filtered" ["a"; "b"] !output


let tests = [
  "times out", `Slow, test_run_with_timeout_times_out;
  "finishes in time", `Slow, test_run_with_timeout_succeeds;
  "action are executed in order", `Quick, test_action_order;
  "ignore stops execution", `Quick, test_ignore;
  "filter actions", `Slow, test_actions_filtering;
]
