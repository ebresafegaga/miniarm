type t

val create : Ast.program -> t
val exec : t -> t
val registers : t -> (Ast.reg * int) list
