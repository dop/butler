let run_with_timeout t cmd = Lwt.(
    catch
      (fun () -> pick [Lwt_unix.timeout t; cmd] >|= fun res -> `Ok res)
      (function
        | Lwt_unix.Timeout -> Lwt.return `Timeout
        | exn -> raise exn))

(* let run_sequence cmds = *)
(*   Lwt.return [] *)

let timeout_equal a b =
  match (a, b) with
    | `Timeout, `Timeout -> true
    | (`Ok a), (`Ok b) -> a = b
    | _ -> false

let fmt_timeout fmt = function
  | `Timeout -> Fmt.pf fmt "`Timeout"
  | `Ok _ -> Fmt.pf fmt "`Ok"

let eq =
  Alcotest.testable fmt_timeout timeout_equal

let test_run_with_timeout_times_out () =
  Alcotest.(check eq) "times out"
    (`Timeout)
    (Lwt_main.run (run_with_timeout 0.1 (Lwt_unix.sleep 1.0)))

let test_run_with_timeout_succeeds () =
  Alcotest.(check eq) "finishes before timeout"
    (`Ok ())
    (Lwt_main.run (run_with_timeout 1.0 (Lwt_unix.sleep 0.1)))

(* let test_run_sequence () = *)
(*   let log = ref [] in *)
(*   let command r = log := !log @ [r]; Lwt.return_unit in *)
(*   let commands = [ command 1; command 2; command 3 ] in *)
(*   Alcotest.(check (list int)) "records all results" *)
(*     [1; 2; 3] *)
(*     (Lwt_main.run (run_sequence commands)) *)

let tests =
  [ "times out", `Slow, test_run_with_timeout_times_out
  ; "finishes in time", `Slow, test_run_with_timeout_succeeds
  (* ; "sequence", `Quick, test_run_sequence *)
  ]
