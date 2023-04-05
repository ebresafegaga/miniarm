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

type constant = Int of int
type value = Register of reg | Immediate of constant
type address = Absolute of int | Relative of reg * int

let fp = R11
let sp = R13
let pc = R15

type instr =
  | Add of reg * value * value
  | Sub of reg * value * value
  | Ldr of { dst : reg; src : address }
  | Str of { src : reg; dst : address }
  | Mov of reg * value
  | Push of reg list

type program = instr list
