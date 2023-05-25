(* open Base *)

type expval

val num_to_expval : int -> expval
val bool_to_expval : bool -> expval
val expval_to_num : expval -> int
val expval_to_bool : expval -> bool