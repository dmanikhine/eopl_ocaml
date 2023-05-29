type env


val make_Empty_env : env
val make_Extend_env :(char * int) list -> env -> env
val apply_env : char ->env -> int
val has_bindingQ : char -> env-> bool
val is_empty_envQ: env -> bool