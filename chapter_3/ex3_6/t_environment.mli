open T_expval

type env


val make_Empty_env : env
val make_Extend_env : char -> expval -> env -> env
val apply_env : env -> char -> expval
val has_bindingQ : env -> char -> bool