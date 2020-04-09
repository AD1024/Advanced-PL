type t

val init : unit -> t
val close : t -> unit

val raw_send : t -> string -> unit
val raw_read_line : t -> string

