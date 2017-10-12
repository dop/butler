open Cohttp_lwt

val run : string -> config_file:string -> unit

val make_file_server : string -> Request.t -> 'a -> (Response.t * Body.t) Lwt.t

