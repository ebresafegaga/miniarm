

type state

type stack

let int_to_byte n =

  let b = Bytes.create 1 in
  Bytes.set b 0 (char_of_int n);
  b

let a = Bytes.blit

let b = Bytes.create 10

let c = Bytes.set_int32_le b 0 23l