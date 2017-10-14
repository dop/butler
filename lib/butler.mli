open Cohttp_lwt

val make_file_server : string -> Request.t -> 'a -> (Response.t * Body.t) Lwt.t

val run : ?serve:bool -> config_file:string -> string -> unit
