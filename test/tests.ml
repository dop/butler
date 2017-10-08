open Core
open Cohttp_lwt_unix

let http_get uri =
  let open Lwt in
  Lwt_main.run begin
    Client.get uri
    >>= fun (resp, body) -> Cohttp_lwt.Body.to_string body
  end

let with_server port spec test =
  let server = Server.make ~callback:(fun _ req body -> spec req body) () in
  let uri = Uri.of_string ("http://0.0.0.0:" ^ (string_of_int port)) in
  let server_failed, server_failed_wake = Lwt.task () in
  let server = Lwt.catch
                 (fun () -> Server.create ~mode:(`TCP (`Port port)) server)
                 (function
                   | Lwt.Canceled -> Lwt.return_unit
                   | x -> Lwt.wakeup_exn server_failed_wake x; Lwt.fail x)
  in
  Lwt.(pick [ test uri; server_failed ] >|= fun res -> cancel server; res)

let serve_index () =
  Lwt_main.run (
    with_server 8080
      (fun _ _ ->
         let body = Sexp.to_string [%sexp ["index.html"; "style.css"; "main.js"]] in
         Server.respond_string ~status:`OK ~body ())
      (fun uri ->
         Lwt.return (
           Alcotest.(check (list string)) "correct list of files"
             ["index.html"; "style.css"; "main.js"]
             (http_get uri
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
