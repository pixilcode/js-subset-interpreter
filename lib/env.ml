open Core

type ident = string
type table = (ident, Heap.address) Hashtbl.t
type t =
  | Top of table
  | Child of table * t

let empty () = Top (Hashtbl.create (module String))

let rec copy parent =
  match parent with
  | Top table -> Top (Hashtbl.copy table)
  | Child (table, parent) -> Child (
    Hashtbl.copy table,
    copy parent
  )

let with_parent parent = Child (Hashtbl.create (module String), parent)

let get_value_from_heap ident table heap =
  let open Option.Monad_infix in

  Hashtbl.find table ident >>| fun (address) ->
  let value = Heap.get ~address heap in
  value

let rec get ~ident ~heap env = 
  match env with
  | Top table -> get_value_from_heap ident table heap
  | Child (table, parent) ->
    match get_value_from_heap ident table heap with
    | Some value -> Some value
    | None -> get ~ident ~heap parent 

let set ~ident ~value ~heap env =
  let address, heap = Heap.add ~value heap in
  match env with
  | Top table ->
    Hashtbl.set ~key:ident ~data:address table;
    (Top table, heap)
  | Child (table, parent) ->
    Hashtbl.set ~key:ident ~data:address table;
    (Child (table, parent), heap)
