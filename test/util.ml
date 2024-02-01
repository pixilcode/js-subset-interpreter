let run_acorn js =
  (* write the js to a temp file *)
  let temp_js_file = Filename.temp_file "acorn" ".js" in
  let temp_json_file = Filename.temp_file "acorn" ".json" in
  let oc = open_out temp_js_file in
  output_string oc js;
  close_out oc;

  (* run acorn on the temp file *)
  ignore (Sys.command ("acorn --ecma2024 " ^ temp_js_file ^ " > " ^ temp_json_file) : int);

  (* read the result *)
  let ic = open_in_bin temp_json_file in
  let json = really_input_string ic (in_channel_length ic) in
  close_in ic;

  (* delete the temp files *)
  Sys.remove temp_js_file;
  Sys.remove temp_json_file;

  json
