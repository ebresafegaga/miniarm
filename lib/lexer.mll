{
    open Lexing 
    open Grammar

    exception SyntaxError of Lexing.position * string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with 
                pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1 }
}

let digit = ['0'-'9']
let int = '-'? digit+
let whitespace = [' ' '\t']+
let newline = '\r'+ | '\n'+ | "\r\n"+

rule read_token = parse
    | "fp" { R11 }
    | "sp" { R13 }
    | "lr" { R14 }
    | "pc" { R15 }
    | "r0" { R0 }
    | "r1" { R1 }
    | "r2" { R2 }
    | "r3" { R3 }
    | "r4" { R4 }
    | "r5" { R5 }
    | "r6" { R6 }
    | "r7" { R7 }
    | "r8" { R8 }
    | "r9" { R9 }
    | "r10" { R10 }
    | "r11" { R11 }
    | "r12" { R12 }
    | "r13" { R13 }
    | "r14" { R14 }
    | "r15" { R15 }
    | "s0" { S0 }
    | "s1" { S1 }
    | "s2" { S2 }
    | "s3" { S3 }
    | "s4" { S4 }
    | "s5" { S5 }
    | "s6" { S6 }
    | "s7" { S7 }
    | "s8" { S8 }
    | "s9" { S9 }
    | "s10" { S10 }
    | "s11" { S11 }
    | "s12" { S12 }
    | "s13" { S13 }
    | "s14" { S14 }
    | "S15" { R15 }
    | "ldr" { LDR }
    | "str" { STR }
    | "mov" { MOV }
    | "add" { ADD }
    | "sub" { SUB }
    | "mul" { MUL }
    | "div" { DIV }
    | "vldr" { VLDR }
    | "vstr" { VSTR }
    | "add.f32" { ADDF32 }
    | "sub.f32" { SUBF32 }
    | "mul.f32" { MULF32 }
    | "div.f32" { DIVF32 }
    | "push" { PUSH }
    | "pop" { POP }
    | "bx" { BX }
    | "!" { BANG }
    | "[" { LBRACK }
    | "]" { RBRACK }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "," { COMMA }
    | whitespace { read_token lexbuf }
    | newline { next_line lexbuf; NEWLINE }
    | "#" { read_number lexbuf }
    | eof { EOF }

and read_number = parse
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError (lexbuf.lex_curr_p, "Expected a number after a #")) }