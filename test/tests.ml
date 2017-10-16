let () =
  Alcotest.run "Butler"
    [ "server", Server_tests.tests
    ; "command runner", Command_runner_tests.tests
    ; "watcher", Watcher_tests.tests
    ]
