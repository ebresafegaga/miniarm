let () =
  let open Miniarm in
  let file = ref "example.arm" in
  let arg = ("-f", Arg.Set_string file, "Specify the file to execute") in
  Arg.parse [ arg ] (Fun.const ()) "miniarm -f [FILE]";
  match !file |> Parser.from_file with
  | Result.Ok program ->
      let machine = Machine.create program in
      let machine = Machine.exec machine in
      let regs = Machine.registers machine in
      regs
      |> List.iter (fun (reg, value) ->
             Printf.printf "%s = %d\n" (Ast.show_reg reg) value)
  | Result.Error e ->
      Printf.eprintf
        "Couldn't parse your program correctly. Did you enter an instruction \
         that isn't supported yet?\n";

      Printf.eprintf
        "Also it might be helpful to remove blank lines from your program\n\n";

      Printf.eprintf "This is the message I got from the parser: \n";

      Printf.eprintf "%s\n" e
  | exception e ->
      let e = Printexc.to_string e in
      Printf.eprintf
        "The lexer couldn't properly tokenize your program. Did you use an \
         instruction that is not yet supported?\n";

      Printf.eprintf "This is the message I got: \n";

      Printf.eprintf "%s" e
