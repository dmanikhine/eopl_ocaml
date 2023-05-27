
type expval

val num_to_expval : int -> expval
val bool_to_expval : bool -> expval
val expval_to_num : expval -> int
val expval_to_bool : expval -> bool
val expval_to_car : expval -> expval
val expval_to_cdr : expval -> expval


val make_Cons_val : expval -> expval -> expval
val make_Emptylist_val : expval
val print_Cons_val : expval -> unit
val emptylistQ :expval -> expval
val list_val : expval list ->expval