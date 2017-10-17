open Core
open Cohttp_lwt_unix

let make_file_server root =
  let ok body =
    Server.respond_string ~status:`OK ~body ()
  in
  let serve_file fname =
    Server.respond_file ~fname ()
  in
  let not_found () =
    Server.respond_string ~status:`Not_found ~body:"Not found" ()
  in
  print_endline ("serving " ^ root);
  let len = String.length root in
  let drop_root filepath =
    String.drop_prefix filepath len
  in
  fun req _ ->
    let uri_path = String.drop_prefix (Uri.path (Request.uri req)) 1 in
    let files = FileUtil.ls root in
    if String.length uri_path > 0 then
      match List.find ~f:(String.is_suffix ~suffix:uri_path) files with
      | Some filepath ->
        serve_file filepath
      | None ->
        not_found ()
    else
      ok (Sexp.to_string (sexp_of_list sexp_of_string (List.map ~f:drop_root files)))

let run port handler =
  let server = Server.make ~callback:(fun _ -> handler) () in
  Server.create ~mode:(`TCP (`Port port)) server
