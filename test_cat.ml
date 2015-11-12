
module X = Xtmpl_xml

let () =
  try
    if Array.length Sys.argv < 2 then
      failwith (Printf.sprintf "Usage: %s <file>" Sys.argv.(0));


    let doc = X.doc_from_file Sys.argv.(1) in
    print_endline (X.doc_to_string doc)
  with
    Failure msg ->
      prerr_endline msg ; exit 1
  | X.Error e ->
      prerr_endline (X.string_of_error e);
      exit 1
  