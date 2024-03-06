type ident = string
type 'value t

val empty: unit -> 'value t
val copy: 'value t -> 'value t
val with_parent: 'value t -> 'value t
val get: ident:ident -> 'value t -> 'value option
val set: ident:ident -> value:'value -> 'value t -> 'value t