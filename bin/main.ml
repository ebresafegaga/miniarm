let () =
  let open Miniarm in
  let file = ref "example.arm" in
  let arg = ("-f", Arg.Set_string file, "Specify the file to execute") in
  Arg.parse [ arg ] (Fun.const ()) "miniarm -f [FIlE]";
  match !file |> Parser.from_file with
  | Result.Ok program ->
      let machine = Machine.create program in
      let machine = Machine.exec machine in
      let regs = Machine.registers machine in
      regs
      |> List.iter (fun (reg, value) ->
             Printf.printf "%s = %d\n" (Ast.show_reg reg) value)
  | Result.Error _ -> Printf.eprintf "An error occured"
