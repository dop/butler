open Core
open Cohttp_lwt_unix
open Tyxml

let html_listing files =
  Html.(
    html
      (head (title (pcdata "Index")) [])
      (body [
          ul (List.map files ~f:(fun file ->
              li [a ~a:[a_href file] [pcdata file]]
            ));
        ]
      ))

let html_to_string doc =
  Format.asprintf "%a" (Html.pp ()) doc

let make_file_server root =
  let html body =
    let headers = Cohttp.Header.of_list [
        "Content-Type", "text/html; charset=utf-8"
      ] in
    Server.respond_string ~headers ~status:`OK ~body ()
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
    String.drop_prefix filepath (len + 1)
  in
  fun req _ ->
    let uri_path = String.drop_prefix (Uri.path (Request.uri req)) 1 in
    let files = FileUtil.ls root in
    if String.length uri_path > 0 then (
      match List.find ~f:(String.is_suffix ~suffix:uri_path) files with
      | Some filepath ->
        if String.is_suffix ~suffix:".html" filepath then
          html (Butler_livereload.html_inject_script (In_channel.read_all filepath))
        else
          serve_file filepath
      | None ->
        not_found ()
    ) else (
      let names = List.map ~f:drop_root files in
      let safe_names = List.filter ~f:(fun name -> name.[0] <> '.') names in
      html (html_to_string (html_listing safe_names))
    )

let run port handler =
  let server = Server.make ~callback:(fun _ -> handler) () in
  Server.create ~mode:(`TCP (`Port port)) server
