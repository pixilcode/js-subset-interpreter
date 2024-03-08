open Core

type 'value t = 'value list
type address = int

let empty () = []

let address_to_list_index address heap =
  let heap_length = List.length heap in
  let dist_from_end = address in
  dist_from_end - heap_length - 1

let out_of_bounds_error_message address heap =
  let index = address_to_list_index address heap in
    Printf.sprintf
      "Address %#x (index %#d) is outside of heap size %#d"
      address
      index
      (List.length heap)

let add_value ~value heap =
  let value_address = List.length heap in
  (* value is appended to front for low addition cost,
     address is distance from the *end* *)
  let new_heap = value :: heap in
  (value_address, new_heap)

let set_value ~address ~value heap =
  let initial_index = address_to_list_index address heap in

  let rec set_value ~index ~value heap =
    match heap with
    | [] ->
      let failure_message = out_of_bounds_error_message address heap in
      failwith failure_message
    | head :: rest ->
      if index = 0 then
        value :: rest
      else
        let next_index = index - 1 in
        head :: (set_value ~value ~index:next_index heap)
  in

  set_value ~value ~index:initial_index heap

let get_value ~address heap =
  let initial_index = address_to_list_index address heap in

  let rec get_value ~index heap =
    if index = 0 then
      List.hd_exn heap
    else
      let next_index = index - 1 in
      get_value ~index:next_index heap
  in

  if initial_index >= List.length heap then
    let failure_message = out_of_bounds_error_message address heap in
    failwith failure_message
  else
    get_value ~index:initial_index heap
  