type t = {
  registers : (Ast.reg, int) Hashtbl.t;
  stack : int Array.t;
  code : Ast.program;
}

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
        (R13, 0);
        (R14, 0);
        (R15, 0);
      ]
    |> List.to_seq |> Hashtbl.of_seq
  in
  let stack = Array.make 1000 0 in
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
let normalize offset = -(offset / alignment)

(* be careful about stack slots *)
let reference : Ast.address -> t -> int * t * int =
 fun address machine ->
  match address with
  | Ast.Absolute i -> (machine.stack.(i), machine, i)
  | Ast.Relative (`None, reg, offset) ->
      let index = register reg machine in
      let offset = normalize offset in
      let address = index + offset in
      (machine.stack.(address), machine, address)
  | Ast.Relative (`Pre, reg, offset) ->
      let offset = normalize offset in
      let machine = update reg machine (fun x -> x + offset) in
      let address = register reg machine in
      (machine.stack.(address), machine, address)
  | Ast.Relative (`Post, reg, offset) ->
      let offset = normalize offset in
      let index = register reg machine in
      let address = index in
      let result = machine.stack.(address) in
      let machine = update reg machine (fun x -> x + offset) in
      (result, machine, address)

let store_at_reference : Ast.address -> t -> int -> t =
 fun address machine v ->
  let _, _, slot = reference address machine in
  machine.stack.(slot) <- v;
  machine

let step_pc machine =
  let pc = register Ast.pc machine in
  let instruction = List.nth_opt machine.code pc in
  let machine = store Ast.pc machine (pc + 1) in
  (machine, instruction)

let registers machine = machine.registers |> Hashtbl.to_seq |> List.of_seq

let rec exec machine =
  let machine, instruction = step_pc machine in
  let instruction = instruction |> Option.get in
  match instruction with
  | Ast.Add (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a + b in
      let machine = store destination machine result in
      exec machine
  | Ast.Sub (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a - b in
      let machine = store destination machine result in
      exec machine
  | Ast.Mul (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a * b in
      let machine = store destination machine result in
      exec machine
  | Ast.Div (destination, a, b) ->
      let a = register a machine in
      let b = value b machine in
      let result = a / b in
      let machine = store destination machine result in
      exec machine
  | Ast.Mov (reg, a) ->
      let machine = store reg machine (value a machine) in
      exec machine
  | Ast.Push regs ->
      let vs = regs |> List.map (fun reg -> register reg machine) in
      let rvs = List.combine regs vs in
      let machine = push rvs machine in
      exec machine
  | Ast.Pop regs ->
      let machine = pop regs machine in
      exec machine
  | Ast.Ldr (destination, source) ->
      let v, machine, _ = reference source machine in
      let machine = store destination machine v in
      exec machine
  | Ast.Str (source, destination) ->
      let machine =
        store_at_reference destination machine (register source machine)
      in
      exec machine
  | Ast.Bx _ -> machine
