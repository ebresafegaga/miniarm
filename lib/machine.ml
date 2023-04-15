type t = {
  registers : (Ast.reg, Num.t) Hashtbl.t;
  stack : Num.t option Array.t;
  code : Ast.program;
}

let registers machine = machine.registers |> Hashtbl.to_seq |> List.of_seq

type error = InvalidMemoryAccess | IncompleteInstruction [@@deriving show]

exception ExecutionError of error

let create code =
  let registers =
    Ast.
      [
        (R0, 0);
        (S0, 0);
        (R1, 0);
        (S1, 0);
        (R2, 0);
        (S2, 0);
        (R3, 0);
        (S3, 0);
        (R4, 0);
        (S4, 0);
        (R5, 0);
        (S5, 0);
        (R6, 0);
        (S6, 0);
        (R7, 0);
        (S7, 0);
        (R8, 0);
        (S8, 0);
        (R9, 0);
        (S9, 0);
        (R10, 0);
        (S10, 0);
        (R11, 0);
        (S11, 0);
        (R12, 0);
        (S12, 0);
        (* sp *)
        (R13, 500);
        (S13, 0);
        (R14, 0);
        (S14, 0);
        (R15, 0);
        (S15, 0);
      ]
    |> List.map (fun (r, v) -> (r, Num.of_int v))
    |> List.to_seq |> Hashtbl.of_seq
  in
  let stack = Array.make 1000 None in
  { registers; stack; code }

let print : Format.formatter -> t -> unit =
 fun formatter machine ->
  Format.pp_set_margin formatter 30;
  Format.open_box 0;
  let pp_rv formatter (reg, value) =
    let value =
      match Ast.is_floating reg with
      | true -> Printf.sprintf "%.2f" (Num.to_float value)
      | false -> value |> Num.to_int |> Int.to_string
    in
    Format.fprintf formatter "%s = %s" (Ast.show_reg reg) value;
    Format.print_cut ()
  in
  let pp_rv_pair formatter (a, b) =
    pp_rv formatter a;
    Format.pp_print_space formatter ();
    pp_rv formatter b
  in
  let regs = registers machine in
  let fs, is =
    regs |> List.partition (fun (r, _) -> Ast.is_floating r) |> fun (fs, is) ->
    let compare (a, _) (b, _) = Ast.compare_reg a b in
    (List.sort compare fs, List.sort compare is)
  in
  let regs = List.combine fs is in
  Format.pp_print_list ~pp_sep:Format.pp_print_space pp_rv_pair formatter regs;
  Format.close_box ()

let register : Ast.reg -> t -> Num.t =
 fun reg machine ->
  (* this is safe: we expect registers to always have values *)
  reg |> Hashtbl.find_opt machine.registers |> Option.get

let value : Ast.value -> t -> Num.t =
 fun value machine ->
  match value with
  | Ast.Register reg -> register reg machine
  | Ast.Immediate (Ast.Int n) -> Num.of_int n
  | Ast.Immediate (Ast.Float n) -> Num.of_float n

let store : Ast.reg -> t -> Num.t -> t =
 fun reg machine value ->
  Hashtbl.replace machine.registers reg value;
  machine

let update : Ast.reg -> t -> (Num.t -> Num.t) -> t =
 fun reg machine updater ->
  let a = register reg machine in
  let b = updater a in
  store reg machine b

(*TODO: push and pop*)
let push : (Ast.reg * Num.t) list -> t -> t = fun _regs_val machine -> machine
let pop : Ast.reg list -> t -> t = fun _regs machine -> machine
let alignment = 4
let normalize offset = offset / alignment

let stack_slot machine index =
  let index = index |> Num.to_int in
  match machine.stack.(index) with
  | Some value -> value
  | None -> 0 |> Num.of_int
  | exception Invalid_argument _ -> raise @@ ExecutionError InvalidMemoryAccess

let place_in_stack_slot machine ~index ~value =
  let index = index |> Num.to_int in
  match machine.stack.(index) <- Some value with
  | () -> machine
  | exception Invalid_argument _ -> raise @@ ExecutionError InvalidMemoryAccess

(* be careful about stack slots *)
let reference : Ast.address -> t -> Num.t * t * Num.t =
 fun address machine ->
  match address with
  | Ast.Absolute i -> (stack_slot machine (Num.of_int i), machine, Num.of_int i)
  | Ast.Relative (`None, reg, offset) ->
      let index = register reg machine in
      let offset = normalize offset in
      let address = Num.to_int index + offset in
      (stack_slot machine @@ Num.of_int address, machine, Num.of_int address)
  | Ast.Relative (`Pre, reg, offset) ->
      let offset = normalize offset in
      let machine =
        update reg machine (fun x -> Num.of_int @@ (Num.to_int x + offset))
      in
      let address = register reg machine in
      (stack_slot machine address, machine, address)
  | Ast.Relative (`Post, reg, offset) ->
      let offset = normalize offset in
      let index = register reg machine in
      let address = index in
      let result = stack_slot machine address in
      let machine =
        update reg machine (fun x -> Num.of_int @@ (Num.to_int x + offset))
      in
      (result, machine, address)

let store_at_reference : Ast.address -> t -> Num.t -> t =
 fun address machine v ->
  let _, _, slot = reference address machine in
  let machine = place_in_stack_slot machine ~index:slot ~value:v in
  machine

let next machine =
  let pc = register Ast.pc machine in
  let instruction = List.nth_opt machine.code (Num.to_int pc) in
  let machine = store Ast.pc machine (Num.Ints.add1 pc) in
  (machine, instruction)

type 'a stepper = Stop of 'a | Continue of 'a

let step : t -> Ast.instr -> t stepper =
 fun machine instr ->
  match instr with
  | Ast.AddF32 (destination, a, b) ->
      let open Num.Floats in
      let a = register a machine in
      let b = value b machine in
      let result = a + b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Add (destination, a, b) ->
      let open Num.Ints in
      let a = register a machine in
      let b = value b machine in
      let result = a + b in
      let machine = store destination machine result in
      Continue machine
  | Ast.SubF32 (destination, a, b) ->
      let open Num.Floats in
      let a = register a machine in
      let b = value b machine in
      let result = a - b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Sub (destination, a, b) ->
      let open Num.Ints in
      let a = register a machine in
      let b = value b machine in
      let result = a - b in
      let machine = store destination machine result in
      Continue machine
  | Ast.MulF32 (destination, a, b) ->
      let open Num.Floats in
      let a = register a machine in
      let b = value b machine in
      let result = a * b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Mul (destination, a, b) ->
      let open Num.Ints in
      let a = register a machine in
      let b = value b machine in
      let result = a * b in
      let machine = store destination machine result in
      Continue machine
  | Ast.DivF32 (destination, a, b) ->
      let open Num.Floats in
      let a = register a machine in
      let b = value b machine in
      let result = a / b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Div (destination, a, b) ->
      let open Num.Ints in
      let a = register a machine in
      let b = value b machine in
      let result = a / b in
      let machine = store destination machine result in
      Continue machine
  | Ast.Mov (reg, a) | Ast.Vmov (reg, a) ->
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
  | Ast.Ldr (destination, source) | Ast.Vldr (destination, source) ->
      let v, machine, _ = reference source machine in
      let machine = store destination machine v in
      Continue machine
  | Ast.Str (source, destination) | Ast.Vstr (source, destination) ->
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
