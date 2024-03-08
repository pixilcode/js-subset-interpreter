open Core

type 'value t = 'value list
type address = int

let empty () = []

let address_to_list_index address heap =
  let heap_length = List.length heap in
  let dist_from_end = address in
  heap_length - dist_from_end - 1

let out_of_bounds_error_message address heap =
  let index = address_to_list_index address heap in
    Printf.sprintf
      "Address %#x (index %#d) is outside of heap size %#d"
      address
      index
      (List.length heap)

let add ~value heap =
  let value_address = List.length heap in
  (* value is appended to front for low addition cost,
     address is distance from the *end* *)
  let new_heap = value :: heap in
  (value_address, new_heap)

let set ~address ~value heap =
  let initial_index = address_to_list_index address heap in

  let rec set ~index ~value heap =
    match heap with
    | [] ->
      let failure_message = out_of_bounds_error_message address heap in
      failwith failure_message
    | head :: rest ->
      if index = 0 then
        value :: rest
      else
        let next_index = index - 1 in
        head :: (set ~value ~index:next_index rest)
  in

  set ~value ~index:initial_index heap

let get ~address heap =
  let initial_index = address_to_list_index address heap in

  let rec get ~index heap =
    if index = 0 then
      List.hd_exn heap
    else if index < 0 then
      failwith "unreachable"
    else
      let next_index = index - 1 in
      let heap_tail = List.tl_exn heap in
      get ~index:next_index heap_tail
  in

  if initial_index < 0 || initial_index >= List.length heap then
    let failure_message = out_of_bounds_error_message address heap in
    failwith failure_message
  else
    get ~index:initial_index heap
 