
type category_status 

type current_player = | One | Two

type t

val init_state : Jeopardy.t -> t

(** [current_categories st] gets the categories left field from [st]. *)  
val current_categories : t -> Jeopardy.category_name list

(** [current_player1_score st] gets the current score of player 1 from [st]. *)
val current_player1_score : t -> int

(** [current_player2_score st] gets the current score of player 2 from [st]. *)
val current_player2_score : t -> int

(** [current_board st] gets the current score from [st]*)
val current_board : t -> string

(** [get_current_player st] gets the current player in [st]. *)
val get_current_player : t -> current_player

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

(** [hint cat lev jeop st] is [r] if requesting a hint for question [lev] in
    category [cat]. If [lev] or [cat] don't exist the result is [Illegal],
    otherwise the player is subtracted 100 points for asking for a hint.
    Hint does NO printing *)
val hint : 
  Jeopardy.category_name -> int -> Jeopardy.t -> t -> result