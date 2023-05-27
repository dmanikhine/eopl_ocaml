open T_environment
open T_expval

type expression

val make_Const_exp: int-> expression
val make_ZeroQ_exp: expression -> expression
val make_Diff_exp: expression -> expression -> expression
val make_Var_exp: char -> expression
val make_If_exp: expression -> expression -> expression->expression
val make_Let_exp: char -> expression -> expression -> expression
val make_Minus_exp: expression -> expression
val make_Add_exp: expression->expression->expression
val make_Mult_exp: expression->expression->expression
val make_Div_exp: expression->expression->expression
val make_EqualQ_exp: expression->expression->expression
val make_GreaterQ_exp: expression->expression->expression
val make_LessQ_exp: expression->expression->expression
val make_Emptylist_exp: expression
val make_Cons_exp: expression->expression->expression
val make_Car_exp: expression->expression
val make_Cdr_exp: expression->expression
val make_NullQ_exp: expression->expression

val value_of: expression -> env -> expval