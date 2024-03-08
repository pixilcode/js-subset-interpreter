type 'value t
type address

val empty : unit -> 'value t

val add : value:'value -> 'value t -> address * 'value t
val set : address:address -> value:'value -> 'value t -> 'value t
val get : address:address -> 'value t -> 'value