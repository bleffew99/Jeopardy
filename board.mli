(** [get_max_cat_length jeop] is the length of the largest category in [jeop]*)
val get_max_cat_length : Jeopardy.t -> int

val game_board : Jeopardy.t -> int list list-> string