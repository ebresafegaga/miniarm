open Miniarm

module Parser_tests = struct
  module To_test = Parser.To_test

  let mov = {|mov r0, #10
  mov r1, r2
  bx lr |}

  let instr_equal : Ast.instr -> Ast.instr -> bool = ( = )
  let instr_testable = Alcotest.testable Ast.pp_instr instr_equal

  let test_mov () =
    Alcotest.(check @@ result (list @@ instr_testable) string)
      "mov works"
      (Ok Ast.[ Mov (R0, Immediate (Int 10)); Mov (R1, Register R2); Bx `lr ])
      (To_test.from_string mov)
end

let () =
  let open Alcotest in
  run "Miniarm"
    [
      ( "Parser tests",
        [ test_case "Parses mov correctly" `Quick Parser_tests.test_mov ] );
    ]
