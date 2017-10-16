let internal_watch_files dir callback =
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
