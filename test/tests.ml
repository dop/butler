open Core
open Cohttp_lwt_unix

let serve_index () =
  let spec =
    let open Test_utils in
    let files =
      [ File ("index.html", "")
      ; File ("style.css", "")
      ; File ("main.js", "")
      ]
    in
    Directory ("public", files)
  in
  let workdir = Test_utils.make_file_tree spec in
  Lwt_main.run (
    Test_utils.with_server 8080
      (fun _ _ ->
         let files =
           let prefix = FilePath.concat workdir "public" in
           let prefix_len = String.length prefix in
           FileUtil.ls prefix
           |> List.map ~f:(fun name -> String.drop_prefix name prefix_len)
         in
         let body = Sexp.to_string (sexp_of_list sexp_of_string files) in
         Server.respond_string ~status:`OK ~body ())
      (fun uri ->
         Lwt.return (
           Alcotest.(check (list string)) "correct list of files"
             ["/index.html"; "/main.js"; "/style.css"]
             (Test_utils.http_get uri
              |> Sexp.of_string
              |> list_of_sexp string_of_sexp)
         )))

let test_serving =
  [ "index" , `Slow, serve_index
  ]

let () =
  Alcotest.run "Butler" [
    "serving", test_serving
  ]
