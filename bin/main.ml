let () =
  let open Miniarm in
  let file = ref "example.arm" in
  let arg = ("-f", Arg.Set_string file, "Specify the file to execute") in
  Arg.parse [ arg ] (Fun.const ()) "miniarm -f [FILE]";
  match !file |> Parser.from_file with
  | Result.Ok program ->
      let machine = Machine.create program in
      let machine =
        try Machine.exec machine
        with Machine.ExecutionError e ->
          Printf.eprintf "An error occured while executing your program: \n";

          Printf.eprintf "%s" (Machine.show_error e);
          exit 1
      in
      Machine.print Format.std_formatter machine
  | Result.Error e ->
      Printf.eprintf
        "The parser told me they couldn't parse your program correctly. Did \
         you use an instruction that isn't supported yet?\n";

      Printf.eprintf
        "It might be helpful to remove blank lines from your program\n\n";

      Printf.eprintf "This is the message I got: \n";

      Printf.eprintf "%s\n" e
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf
        "The lexer told me they couldn't properly tokenize your program. Did \
         you use an instruction that is not yet supported?\n";

      Printf.eprintf "This is the message I got: \n";

      Printf.eprintf "%s" e
