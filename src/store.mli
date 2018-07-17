type dir = string
type key = string list

type t

val from : dir -> t Lwt.t
val find : t -> key -> string option Lwt.t
val set : t -> key -> string -> unit Lwt.t
