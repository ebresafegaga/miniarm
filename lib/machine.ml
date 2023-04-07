type t = {
  registers : (Ast.reg, int) Hashtbl.t;
  stack : int option Array.t;
  code : Ast.program;
}

type error = InvalidMemoryAccess | IncompleteInstruction [@@deriving show]

exception ExecutionError of error

let create code =
  let registers =
    Ast.
      [
        (R0, 0);
        (R1, 0);
        (R2, 0);
        (R3, 0);
        (R4, 0);
        (R5, 0);
        (R6, 0);
        (R7, 0);
        (R8, 0);
        (R9, 0);
        (R10, 0);
        (R11, 0);
        (R12, 0);
        (* sp *)
        (R13, 500);
        (R14, 0);
        (R15, 0);
      ]
    |> List.to_seq |> Hashtbl.of_seq
  in
  let stack = Array.make 1000 None in
  { registers; stack; code }

let register : Ast.reg -> t -> int =
 fun reg machine ->
  (* this is safe: we expect registers to always have values *)
  reg |> Hashtbl.find_opt machine.registers |> Option.get

let value : Ast.value -> t -> int =
 fun value machine ->
  match value with
  | Ast.Register reg -> register reg machine
  | Ast.Immediate (Ast.Int n) -> n

let store : Ast.reg -> t -> int -> t =
 fun reg machine value ->
  Hashtbl.replace machine.registers reg value;
  machine

let update : Ast.reg -> t -> (int -> int) -> t =
 fun reg machine updater ->
  let a = register reg machine in
  let b = updater a in
  store reg machine b

(*TODO: push and pop*)
let push : (Ast.reg * int) list -> t -> t = fun _regs_val machine -> machine
let pop : Ast.reg list -> t -> t = fun _regs machine -> machine
let alignment = 4
let normalize offset = offset / alignment

let stack_slot machine index =
  match machine.stack.(index) with
  | Some value -> value
  | None -> 0
  | exception Invalid_argument _ -> raise @@ ExecutionError InvalidMemoryAccess

let place_in_stack_slot machine ~index ~value =
  match machine.stack.(index) <- Some value with
  | () -> machine
  | exception Invalid_argument _ -> raise @@ ExecutionError InvalidMemoryAccess

(* be careful about stack slots *)
let reference : Ast.address -> t -> int * t * int =
 fun address machine ->
  match address with
  | Ast.Absolute i -> (stack_slot machine i, machine, i)
  | Ast.Relative (`None, reg, offset) ->
      let index = register reg machine in
      let offset = normalize offset in
      let address = index + offset in
      (stack_slot machine address, machine, address)
  | Ast.Relative (`Pre, reg, offset) ->
      let offset = normalize offset in
      let machine = update reg machine (fun x -> x + offset) in
      let address = register reg machine in
      (stack_slot machine address, machine, address)
  | Ast.Relative (`Post, reg, offset) ->
      let offset = normalize offset in
      let index = register reg machine in
      let address = index in
      let result = stack_slot machine address in
      let machine = update reg machine (fun x -> x + offset) in
      (result, machine, address)

let store_at_reference : Ast.address -> t -> int -> t =
 fun address machine v ->
  let _, _, slot = reference address machine in
  let machine = place_in_stack_slot machine ~index:slot ~value:v in
  machine

let next machine =
  let pc = register Ast.pc machine in
  let instruction = List.nth_opt machine.code pc in
  let machine = store Ast.pc machine (pc + 1) in
  (machine, instruction)

let registers machine = machine.registers |> Hashtbl.to_seq |> List.of_seq

type 'a stepper = Stop of 'a | Continue of 'a

let step : t -> Ast.instr -> t stepper =
 fun machine instr ->
  match instr with
  | Ast.Add (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a + b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Sub (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a - b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Mul (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a * b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Div (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a / b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Mov (reg, a) ->
      let machine = store reg machine (value a machine) in
      Continue machine
  | Ast.Push regs ->
      let vs = regs |> List.map (fun reg -> register reg machine) in
      let rvs = List.combine regs vs in
      let machine = push rvs machine in
      Continue machine
  | Ast.Pop regs ->
      let machine = pop regs machine in
      Continue machine
  | Ast.Ldr (destination, source) ->
      let v, machine, _ = reference source machine in
      let machine = store destination machine v in
      Continue machine
  | Ast.Str (source, destination) ->
      let machine =
        store_at_reference destination machine (register source machine)
      in
      Continue machine
  | Ast.Bx _ -> Stop machine

let rec exec machine =
  let m, instr = next machine in
  let instr =
    match instr with
    | Some x -> x
    | None -> raise @@ ExecutionError IncompleteInstruction
  in
  match step m instr with Continue m -> exec m | Stop m -> m
