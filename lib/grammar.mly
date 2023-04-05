%token LDR
%token STR
%token MOV
%token ADD
%token SUB
%token PUSH

%token LBRACE LBRACK
%token RBRACE RBRACK

%token COMMA
%token NEWLINE
%token <int> INT
%token EOF

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

program: instructions = separated_list(NEWLINE, instruction); EOF { instructions }

instruction: 
    | MOV; destination = register; COMMA; source = value { Ast.Mov (destination, source) }

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