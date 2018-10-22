type category_status

type t

val init_state : Jeopardy.t -> t

val current_categories : t -> Jeopardy.category_name list

val current_score : t -> int

val current_category_levels : t -> Jeopardy.category_name -> int list

val current_category_levels_to_string : t -> string

type result = Legal of t | Illegal

val play : Jeopardy.category_name -> int -> Jeopardy.t -> t -> result