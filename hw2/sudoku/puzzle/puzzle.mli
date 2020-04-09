type t

exception ParseError of string

val from_channel : Scanf.Scanning.in_channel -> t
val show : t -> string
