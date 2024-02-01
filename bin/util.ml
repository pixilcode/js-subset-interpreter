let rec read_stdin () =
  try
    let line = input_line stdin in
    line ^ "\n" ^ read_stdin ()
  with
    End_of_file -> ""