type reg =
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | S12
  | S13
  | S14
  | S15
[@@deriving show]

type constant =
  | Int of int
  | Float of float

type value =
  | Register of reg
  | Immediate of constant

type address =
  | Absolute of int
  | Relative of (* Indexing: *) [ `Post | `Pre | `None ] * reg * int

let fp = R11
let sp = R13
let lr = R14
let pc = R15

type instr =
  | Add of reg * reg * value
  | Sub of reg * reg * value
  | Mul of reg * reg * value
  | Div of reg * reg * value
  | Ldr of reg * address
  | Str of reg * address
  | Mov of reg * value
  | AddF32 of reg * reg * value
  | SubF32 of reg * reg * value
  | MulF32 of reg * reg * value
  | DivF32 of reg * reg * value
  | Vldr of reg * address
  | Vstr of reg * address
  | Push of reg list
  | Pop of reg list
  | Bx of [ `lr ]

type program = instr list