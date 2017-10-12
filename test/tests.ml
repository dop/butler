open Core
open Cohttp_lwt_unix

let setup ?additional_files () =
  let open Test_utils in
  let files =
    [ File ("index.html", "<!doctype><html></html>")
    ; File ("style.css", "")
    ; File ("main.js", "")
    ]
  in
  Test_utils.make_file_tree (
    Directory (
      "public",
      files @ (Option.value ~default:[] additional_files)
    ))

let make_file_server dir =
  let ok body =
    Server.respond_string ~status:`OK ~body ()
  in
  let serve_file fname =
    Server.respond_file ~fname ()
  in
  let not_found () =
    Server.respond_string ~status:`Not_found ~body:"Not found" ()
  in
  print_endline ("serving " ^ dir);
  let root = FilePath.concat dir "public" in
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

let serve_index () =
  let workdir = setup () in
  Lwt_main.run (
    Test_utils.with_server 8080 (make_file_server workdir)
      (fun uri ->
         Alcotest.(check (list string)) "correct list of files"
           ["/index.html"; "/main.js"; "/style.css"]
           (Test_utils.http_get uri
            |> Sexp.of_string
            |> list_of_sexp string_of_sexp)
      ))

let serve_file () =
  let contents = "<!doctype><html></html>" in
  let workdir = setup ~additional_files:[File ("index.html", contents)] () in
  Lwt_main.run (
    Test_utils.with_server 8080 (make_file_server workdir)
      (fun uri ->
         Alcotest.(check string) "correct file contents"
           contents
           (Test_utils.http_get (Uri.with_path uri "index.html"))
      ))

let test_serving =
  [ "index", `Slow, serve_index
  ; "file", `Slow, serve_file
  ]

let () =
  Alcotest.run "Butler" [
    "serving", test_serving
  ]
