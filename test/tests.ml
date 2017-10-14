open Core
(* open Cohttp_lwt_unix *)

let ( / ) = FilePath.concat

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

let serve_index () =
  let workdir = setup () / "public" in
  Lwt_main.run (
    Test_utils.with_server 8080 (Butler.make_file_server workdir)
      (fun uri ->
         Alcotest.(check (list string)) "correct list of files"
           ["/index.html"; "/main.js"; "/style.css"]
           (Test_utils.http_get uri
            |> Sexp.of_string
            |> list_of_sexp string_of_sexp)
      ))

let serve_file () =
  let open Test_utils in
  let contents = "<!doctype><html></html>" in
  let files = [File ("index.html", contents)] in
  let workdir = setup ~additional_files:files () / "public" in
  Lwt_main.run (
    Test_utils.with_server 8080 (Butler.make_file_server workdir)
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
