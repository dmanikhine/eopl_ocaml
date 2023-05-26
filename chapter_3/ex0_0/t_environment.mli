open T_expval

type env


val make_empty_env : env
val make_env : char -> expval -> env -> env
val apply_env : env -> char -> expval
val has_bindingQ : env -> char -> bool