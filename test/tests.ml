let () =
  Alcotest.run "Butler"
    [ "server", Server_tests.tests
    ; "watcher", Watcher_tests.tests
    ]
