open Core
open Cohttp_lwt_unix

type file_tree =
  | Directory of string * file_tree list
  | File of string * string

let with_server port spec test =
  let server = Server.make ~callback:(fun _ req body -> spec req body) () in
  let uri = Uri.of_string ("http://0.0.0.0:" ^ (string_of_int port)) in
  let server_failed, server_failed_wake = Lwt.task () in
  let server =
    Lwt.catch
      (fun () -> Server.create ~mode:(`TCP (`Port port)) server)
      (function
        | Lwt.Canceled -> Lwt.return_unit
        | x -> Lwt.wakeup_exn server_failed_wake x; Lwt.fail x)
  in
  Lwt.(pick [ return (test uri); server_failed ] >|= fun res -> cancel server; res)

let http_get uri =
  let open Lwt in
  Lwt_main.run begin
    Client.get uri
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
  end

let char_range a z =
  let l = Char.to_int a and r = Char.to_int z in
  if l <= r then
    let result = Bytes.create (r - l + 1) in
    List.range ~stop:`inclusive l r
    |> List.iteri ~f:(fun i n -> Bytes.set result i (Char.of_int_exn n));
    result
  else
    ""

let random_string () =
  let source =
    String.concat_array
      [| char_range '0' '9'; char_range 'a' 'z'; char_range 'A' 'Z'; "_-"|] in
  let source_len = String.length source in
  let output = String.make 20 '_' in
  for i = 0 to 19 do
    Bytes.set output i source.[Random.int source_len]
  done;
  output

let make_file_tree spec =
  let ( / ) = FilePath.concat in
  let open FileUtil in
  let rec tmp_file_tree_1 workdir spec =
    match spec with
    | Directory (name, files) ->
      let parent = workdir / name in
      mkdir parent;
      List.iter ~f:(tmp_file_tree_1 parent) files
    | File (name, contents) ->
      Out_channel.write_all ~data:contents (workdir / name)
  in
  let tmpdir = FilePath.make_filename [Sys.getenv_exn "TMPDIR"] in
  let workdir = tmpdir / ("butler-" ^ random_string ()) in
  if not (test Exists workdir) then
    mkdir workdir;
  tmp_file_tree_1 workdir spec;
  workdir
