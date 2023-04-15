type t

val of_int : int -> t
val to_int : t -> int
val of_float : float -> t
val to_float : t -> float

module Ints : sig
  val add1 : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end

module Floats : sig
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
end
