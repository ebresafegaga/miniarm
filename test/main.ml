open Miniarm

module Parser_tests = struct
  module To_test = Parser.To_test

  let create_test_case ~name ~source ~expected () =
    let instr_equal : Ast.instr -> Ast.instr -> bool = ( = ) in
    let instr_testable = Alcotest.testable Ast.pp_instr instr_equal in
    Alcotest.(check @@ result (list @@ instr_testable) string)
      name (Ok expected) (To_test.parse source)

  let test_mov =
    let source = {|mov r0, #10
    mov r1, r2
    bx lr |} in
    create_test_case ~name:"instruction mov"
      ~expected:[ Mov (R0, Immediate (Int 10)); Mov (R1, Register R2); Bx `lr ]
      ~source

  let test_ldr =
    let source =
      {|ldr r9, #10
        ldr r4, #-10
        ldr r11, [sp]
        ldr r5, [r3]
        ldr r8, [r6, #22]
        ldr r7, [r0, #-12]
        ldr r12, [r11, #4]!
        ldr r10, [r3, #-23]!
        ldr r2, [r9], #56 
        ldr r1, [r15], #-62 
        bx lr |}
    in
    create_test_case ~name:"instruction mov"
      ~expected:
        Ast.
          [
            Ldr (R9, Absolute 10);
            Ldr (R4, Absolute (-10));
            Ldr (R11, Relative (`None, R13, 0));
            Ldr (R5, Relative (`None, R3, 0));
            Ldr (R8, Relative (`None, R6, 22));
            Ldr (R7, Relative (`None, R0, -12));
            Ldr (R12, Relative (`Pre, R11, 4));
            Ldr (R10, Relative (`Pre, R3, -23));
            Ldr (R2, Relative (`Post, R9, 56));
            Ldr (R1, Relative (`Post, R15, -62));
            Bx `lr;
          ]
      ~source
end

let () =
  let open Alcotest in
  run "Miniarm"
    [
      ( "Parser tests",
        [
          test_case "Parses mov correctly" `Quick Parser_tests.test_mov;
          test_case "Parses ldr correctly" `Quick Parser_tests.test_ldr;
        ] );
    ]
