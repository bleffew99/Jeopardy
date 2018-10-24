(** [category_status] represents a category's name and the levels unplayed *)
type category_status

(** [t] represents a game state with current categories [categories_left] and
    their statuses represented by [categories]. [score] represents the player's
    current total score. *)
type t

(** [init_state jeop] is the initial state of the game when playing [jeop].
    They have a starting score of 0 *)
val init_state : Jeopardy.t -> t

(** [current_categories st] gets the categories left field from [st]. *)  
val current_categories : t -> Jeopardy.category_name list

(** [current_score st] gets the current score from [st]. *)
val current_score : t -> int

(** [current_category_levels st cat] returns the current levels still unplayed
    in category [cat] *)
val current_category_levels : t -> Jeopardy.category_name -> int list

(** [current_category_levels_to_string st] returns a string containing each 
    category in categories_left of [st] followed by the levels left in that 
    category. *)
val current_category_levels_to_string : t -> string

type result = Legal of t | Illegal

(** [play cat lev jeop st] is [r] if attempting to select the question in
    categoty [cat] and level [lev] in jeopardy [jeop]. If [cat] or [lev]
    don't exist in [jeop] the result is Illegal, otherwise it is [Legal st']
    where [st'] is the players state after asking the question. *)
val play : Jeopardy.category_name -> int -> Jeopardy.t -> t -> result

(** [answer cat lev jeop st] is [r] if attempting to answer the question
    in [cat] and [lev] in [jeop]. Depending on whether or not [ans] is correct,
    the player's score either increases or decreases by the score of [lev].
    If lev or cat don't exist as an unanswered question, the result 
    is [Illegal]. *)
val answer : 
  Jeopardy.category_name -> int -> string -> Jeopardy.t -> t -> result