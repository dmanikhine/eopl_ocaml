open T_environment
open T_expval

type expression

val make_Const_exp: int-> expression
val make_ZeroQ_exp: expression -> expression
val make_Diff_exp: expression -> expression -> expression
val make_Var_exp: char -> expression
val make_If_exp: expression -> expression -> expression->expression
val make_Let_exp: (char * expression) list -> expression -> expression

val value_of: expression -> env -> expval