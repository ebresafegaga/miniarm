%token LDR
%token VLDR
%token STR
%token VSTR
%token MOV VMOV
%token ADD MUL
%token SUB DIV
%token ADDF32 MULF32
%token SUBF32 DIVF32
%token PUSH POP
%token BX

%token LBRACE LBRACK
%token RBRACE RBRACK

%token COMMA
%token NEWLINE
%token <int> INT
%token <float> FLOAT
%token EOF
%token BANG

%token R0
%token R1
%token R2
%token R3
%token R4
%token R5
%token R6
%token R7
%token R8
%token R9
%token R10
%token R11
%token R12
%token R13
%token R14
%token R15

%token S0
%token S1
%token S2
%token S3
%token S4
%token S5
%token S6
%token S7
%token S8
%token S9
%token S10
%token S11
%token S12
%token S13
%token S14
%token S15

%start program

%type <Ast.program> program

%type <Ast.instr> instruction

%%

(* TODO: This is too rigid allow newlines to be in other places *)
program: 
    | instructions = separated_list(NEWLINE, instruction); EOF { instructions }

instruction: 
    | MOV; destination = register; COMMA; source = value { Ast.Mov (destination, source) }
    | VMOV; destination = register; COMMA; source = value { Ast.Vmov (destination, source) }
    | LDR; destination = register; COMMA; source = address { Ast.Ldr (destination, source) }
    | VLDR; destination = register; COMMA; source = address { Ast.Vldr (destination, source) }
    | STR; source = register; COMMA; destination = address { Ast.Str (source, destination) }
    | VSTR; source = register; COMMA; destination = address { Ast.Vstr (source, destination) }
    | ADD; destination = register; COMMA; a = register; COMMA; b = value { Ast.Add (destination, a, b) }
    | SUB; destination = register; COMMA; a = register; COMMA; b = value { Ast.Sub (destination, a, b) }
    | MUL; destination = register; COMMA; a = register; COMMA; b = value { Ast.Mul (destination, a, b) }
    | DIV; destination = register; COMMA; a = register; COMMA; b = value { Ast.Div (destination, a, b) }
    | ADDF32; destination = register; COMMA; a = register; COMMA; b = value { Ast.AddF32 (destination, a, b) }
    | SUBF32; destination = register; COMMA; a = register; COMMA; b = value { Ast.SubF32 (destination, a, b) }
    | MULF32; destination = register; COMMA; a = register; COMMA; b = value { Ast.MulF32 (destination, a, b) }
    | DIVF32; destination = register; COMMA; a = register; COMMA; b = value { Ast.DivF32 (destination, a, b) }
    | PUSH; regs = reg_list; { Ast.Push (regs) }
    | POP; regs = reg_list; { Ast.Pop (regs) }
    (* bx lr *)
    (* link register is r14 *)
    | BX; R14 { Ast.Bx `lr }
    

register: 
    | R0 { Ast.R0 }
    | R1 { Ast.R1 }
    | R2 { Ast.R2 }
    | R3 { Ast.R3 }
    | R4 { Ast.R4 }
    | R5 { Ast.R5 }
    | R6 { Ast.R6 }
    | R7 { Ast.R7 }
    | R8 { Ast.R8 }
    | R9 { Ast.R9 }
    | R10 { Ast.R10 }
    | R11 { Ast.R11 }
    | R12 { Ast.R12 }
    | R13 { Ast.R13 }
    | R14 { Ast.R14 }
    | R15 { Ast.R15 }
    | S0 { Ast.S0 }
    | S1 { Ast.S1 }
    | S2 { Ast.S2 }
    | S3 { Ast.S3 }
    | S4 { Ast.S4 }
    | S5 { Ast.S5 }
    | S6 { Ast.S6 }
    | S7 { Ast.S7 }
    | S8 { Ast.S8 }
    | S9 { Ast.S9 }
    | S10 { Ast.S10 }
    | S11 { Ast.S11 }
    | S12 { Ast.S12 }
    | S13 { Ast.S13 }
    | S14 { Ast.S14 }
    | S15 { Ast.S15 }

value: 
    | r = register { Ast.Register (r) }
    | n = INT { Ast.Immediate (Ast.Int n) }
    | n = FLOAT { Ast.Immediate (Ast.Float n) }

address: 
    (* TODO: support labels *)
    | n = INT { Ast.Absolute (n) }
    (* e.g [r0]  *)
    | LBRACK; r = register; RBRACK { Ast.Relative (`None, r, 0) }
    (* e.g [r1, #2] *)
    | LBRACK; r = register; COMMA; offset = INT; RBRACK { Ast.Relative (`None, r, offset) }
    (* e.g [r1, #4]! *)
    | LBRACK; r = register; COMMA; offset = INT; RBRACK; BANG { Ast.Relative (`Pre, r, offset) }
    (* e.g [r1], #4  *)
    | LBRACK; r = register; RBRACK; COMMA; offset = INT { Ast.Relative (`Post, r, offset) }

reg_list:
    | LBRACE; regs = separated_list(COMMA, register); RBRACE { regs }