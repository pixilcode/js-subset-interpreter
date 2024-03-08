type 'value t
type address

val empty : unit -> 'value t

val add_value : value:'value -> 'value t -> address * 'value t
val set_value : address:address -> value:'value -> 'value t -> 'value t
val get_value : address:address -> 'value t -> 'value