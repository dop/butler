open Cmdliner

let main serve config_file directory =
  Butler.run ~serve ~config_file directory

let main_t =
  let serve = Arg.(
      value & flag & info ["s"; "serve"]
        ~doc:"Start file server."
    )
  in
  let config_file = Arg.(
      value & opt string "butler" & info ["c"; "config"]
        ~docv:"FILE"
        ~doc:"Configuration file."
    )
  in
  let directory = Arg.(
      value & pos 0 string "." & info []
        ~docv:"DIR"
        ~doc:"Directory to run in. Defaults to current directory."
    )
  in
  Term.(const main $ serve $ config_file $ directory)

let info =
  let doc = "run commands on file changes." in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <donatas.petr at gmail.com>." ]
  in
  Term.info "butler" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () =
  Term.exit @@ Term.eval (main_t, info)
