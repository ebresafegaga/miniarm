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
[@@deriving show]

type constant = Int of int
type value = Register of reg | Immediate of constant

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
  | Push of reg list
  | Pop of reg list
  | Bx of [ `lr ]

type program = instr list
