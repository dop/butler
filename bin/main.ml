open Core

let () =
  Butler.run "." (Butler.read_config "butler")
