%token LDR
%token STR
%token MOV
%token ADD MUL
%token SUB DIV
%token PUSH POP
%token BX

%token LBRACE LBRACK
%token RBRACE RBRACK

%token COMMA
%token NEWLINE
%token <int> INT
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

%start program

%type <Ast.program> program

%type <Ast.instr> instruction

%%

(* TODO: This is too rigid allow newlines to be in other places *)
program: 
    | instructions = separated_list(NEWLINE, instruction); EOF { instructions }

instruction: 
    | MOV; destination = register; COMMA; source = value { Ast.Mov (destination, source) }
    | LDR; destination = register; COMMA; source = address { Ast.Ldr (destination, source) }
    | STR; source = register; COMMA; destination = address { Ast.Str (source, destination) }
    | ADD; destination = register; COMMA; a = register; COMMA; b = value { Ast.Add (destination, a, b) }
    | SUB; destination = register; COMMA; a = register; COMMA; b = value { Ast.Sub (destination, a, b) }
    | MUL; destination = register; COMMA; a = register; COMMA; b = value { Ast.Mul (destination, a, b) }
    | DIV; destination = register; COMMA; a = register; COMMA; b = value { Ast.Div (destination, a, b) }
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

value: 
    | r = register { Ast.Register (r) }
    | n = INT { Ast.Immediate (Ast.Int n) }

address: 
    (* TODO: support labels *)
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