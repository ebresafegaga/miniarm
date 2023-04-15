type t = Bytes.t

let of_int n =
  let bytes = Bytes.create 4 in
  Char.chr (n land 0xff) |> Bytes.set bytes 0;
  Char.chr ((n lsr 8) land 0xff) |> Bytes.set bytes 1;
  Char.chr ((n lsr 16) land 0xff) |> Bytes.set bytes 2;
  Char.chr ((n lsr 24) land 0xff) |> Bytes.set bytes 3;
  bytes

let to_int b = Bytes.get_int32_le b 0 |> Int32.to_int
let of_float n = n |> Int32.bits_of_float |> Int32.to_int |> of_int
let to_float b = b |> to_int |> Int32.of_int |> Int32.float_of_bits

module Ints = struct
  let add1 n = of_int (1 + to_int n)
  let add_int a b = of_int (to_int a + to_int b)
  let sub_int a b = of_int (to_int a - to_int b)
  let mul_int a b = of_int (to_int a * to_int b)
  let div_int a b = of_int (to_int a / to_int b)
  let ( + ) = add_int
  let ( - ) = sub_int
  let ( * ) = mul_int
  let ( / ) = div_int
end

module Floats = struct
  let add_int a b = of_float (to_float a +. to_float b)
  let sub_int a b = of_float (to_float a -. to_float b)
  let mul_int a b = of_float (to_float a *. to_float b)
  let div_int a b = of_float (to_float a /. to_float b)
  let ( + ) = add_int
  let ( - ) = sub_int
  let ( * ) = mul_int
  let ( / ) = div_int
end
