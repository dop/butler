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

let get_hrefs str =
  let open Soup in
  (parse str) $$ "a[href]"
  |> to_list
  |> List.fold ~init:[] ~f:(fun hrefs node ->
      match attribute "href" node with
      | Some href -> href :: hrefs
      | None -> hrefs
    )
  |> List.rev

let serve_index () =
  let workdir = setup () / "public" in
  Lwt_main.run (
    Test_utils.with_server 8080 (Butler_server.make_file_server workdir)
      (fun uri ->
         Alcotest.(check (list string)) "correct list of files"
           ["index.html"; "main.js"; "style.css"]
           (Test_utils.http_get uri |> get_hrefs)
      ))

let skip_dot_files () =
  let workdir = setup ~additional_files:[File (".secret", "")] () / "public" in
  Lwt_main.run (
    Test_utils.with_server 8080 (Butler_server.make_file_server workdir)
      (fun uri ->
         Alcotest.(check (list string)) "correct list of files"
           ["index.html"; "main.js"; "style.css"]
           (Test_utils.http_get uri |> get_hrefs)
      ))

let serve_file () =
  let open Test_utils in
  let contents = "<!doctype><html></html>" in
  let files = [File ("index.html", contents)] in
  let workdir = setup ~additional_files:files () / "public" in
  Lwt_main.run (
    Test_utils.with_server 8080 (Butler_server.make_file_server workdir)
      (fun uri ->
         Alcotest.(check string) "correct file contents"
           contents
           (Test_utils.http_get (Uri.with_path uri "index.html"))
      ))

let serve_html () =
  let open Test_utils in
  let contents = "<!doctype><html><body></body></html>" in
  let modified_contents = "<!doctype><html><body><script src=\"/livereload.js\"></script></body></html>" in
  let files = [File ("index.html", contents)] in
  let workdir = setup ~additional_files:files () / "public" in
  Lwt_main.run (
    Test_utils.with_server 8080 (Butler_server.make_file_server workdir)
      (fun uri ->
         Alcotest.(check string) "correct file contents"
           modified_contents
           (Test_utils.http_get (Uri.with_path uri "index.html"))
      ))

let script_tag =
  "<script src=\"/livereload.js\"></script>"

let modify_html () =
  Alcotest.(check string) "added livereload script to head"
    ("<body>" ^ script_tag ^ "</body>")
    (Butler_livereload.html_inject_script "<body></body>")

let serve_websocket () =
  let open Websocket in
  Lwt_main.run Lwt.(
      let server (_, write) =
        write (Frame.create ~content:"hi" ()) in
      Test_utils.with_ws_server 9001 server (fun (read, _) ->
          read () >|= (fun {content; _} ->
              Alcotest.(check string) "correct message" "hi" content
            )
        )
    )

let tests =
  [ "index", `Slow, serve_index
  ; "skip dot files", `Slow, skip_dot_files
  ; "file", `Slow, serve_file
  ; "serving html", `Slow, serve_html
  ; "modifying html", `Quick, modify_html
  ; "websocket", `Slow, serve_websocket
  ]
