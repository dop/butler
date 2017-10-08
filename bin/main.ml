open Core

let () =
  Butler.run ~config_file:"butler" "."
