open Core

let ( / ) = FilePath.concat

let setup () =
  let open Test_utils in
  Test_utils.make_file_tree (Directory ("target", []))

let test_file_change () =
  let workdir = setup () / "target" in
  print_endline workdir;
  Lwt.(async (fun () -> Lwt_unix.sleep 0.1 >|= fun () -> FileUtil.touch (workdir / "file")));
  let changed = Lwt_main.run (Lwt.(
        pick [ Lwt_unix.sleep 1.0 >|= const false
             ; Butler_utils.watch_files workdir (fun waker _ ->
                   Lwt.wakeup waker true;
                   Lwt.return_unit
                 )
             ]
      ))
  in
  Alcotest.(check bool) "file changed" true changed

let tests =
  [ "file changed", `Slow, test_file_change
  ]
