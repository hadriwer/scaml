(* Build-time generator for bin/stdlib_files.ml: embeds each file given on
   the command line as a ("stdlib/<basename>", contents) pair, so the
   compiler finds its stdlib wherever it's run from or installed. *)
let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  let files =
    Array.to_list Sys.argv |> List.tl |> List.sort compare
  in
  print_string "let files = [\n";
  List.iter
    (fun path ->
      Printf.printf "  (%S, %S);\n"
        ("stdlib/" ^ Filename.basename path) (read_file path))
    files;
  print_string "]\n"
