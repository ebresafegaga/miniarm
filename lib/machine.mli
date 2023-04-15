type t
type error

exception ExecutionError of error

val show_error : error -> string
val create : Ast.program -> t
val exec : t -> t
val registers : t -> (Ast.reg * Num.t) list
val print : Format.formatter -> t -> unit
