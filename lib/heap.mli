type t
type address

val empty : unit -> 'a list

val add_value : value:Value.t -> t -> address * t
val set_value : address:address -> value:Value.t -> t -> t
val get_value : address:address -> t