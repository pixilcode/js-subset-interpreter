type ident = string
type t

val empty: unit -> t
val copy: t -> t
val with_parent: t -> t
val get: ident:ident -> heap:'value Heap.t -> t -> 'value option
val set: ident:ident -> value:'value -> heap:'value Heap.t -> t -> t * 'value Heap.t
val update: ident:ident -> value:'value -> heap:'value Heap.t -> t -> 'value Heap.t option