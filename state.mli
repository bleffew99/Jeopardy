(** [category_status] represents a category's name and the levels unplayed *)
type category_status

(** [t] represents a game state with current categories [categories_left] and
    their statuses represented by [categories]. [score] represents the player's
    current total score. [passes_left] represents the number of passes left for
    the player. [board] represents the current game board. *)
type t

(** [init_state jeop] is the initial state of the game when playing [jeop].
    They have a starting score of 0 *)
val init_state : Jeopardy.t -> t

(** [current_categories st] gets the categories left field from [st]. *)  
val current_categories : t -> Jeopardy.category_name list

(** [current_score st] gets the current score from [st]. *)
val current_score : t -> int

(** [current_passes_left st] gets the current number of passes left from [st].*)
val current_passes_left: t -> int

(** [get_final_bet st] gets the final jeopardy bet from [st].*)
val get_final_bet: t -> int

(** [current_board st] get the current score from [st]*)
val current_board : t -> string

(** [has_played_final st] returns true if the final question has been answered
    false otherwise in the state st.*)
val has_played_final : t -> bool 

(** [current_category_levels st cat] returns the current levels still unplayed
    in category [cat] *)
val current_category_levels : t -> Jeopardy.category_name -> int list

type result = Legal of t | Illegal

(** [play cat lev jeop st] is [r] if attempting to select the question in
    categoty [cat] and level [lev] in jeopardy [jeop]. If [cat] or [lev]
    doesn't exist in [jeop] the result is Illegal, otherwise it is [Legal st']
    where [st'] is the player's state after playing the question. *)
val play : Jeopardy.category_name -> int -> Jeopardy.t -> t -> result

(** [answer cat lev jeop st] is [r] if attempting to answer the question
    in [cat] and [lev] in [jeop]. Depending on whether or not [ans] is correct,
    the player's score either increases or decreases by the score of [lev].
    If [lev] or [cat] doesn't exist as an unanswered question, the result 
    is [Illegal]. *)
val answer : 
  Jeopardy.category_name -> int -> string -> Jeopardy.t -> t -> result

(** [hint cat lev jeop st] is [r] if requesting a hint for question [lev] in
    category [cat]. If [lev] or [cat] doesn't exist the result is [Illegal],
    otherwise the player is deducted 100 points for asking for a hint.
    Hint does NO printing. *)
val hint : 
  Jeopardy.category_name -> int -> Jeopardy.t -> t -> result

(** [pass st] is [r] if requesting a pass in state [st]. If the number of passes
    left is 0, then the result is [Illegal] otherwise the player is deducted
    1 pass for asking for a pass. *)
val pass : t -> result

(** [double st] is [r] if requesting to use the double-or-nothing ability in
    state [st]. If used_double is true, then the result is Illegal, otherwise
    used_double in [st] is set to true. *)
val double : t -> Jeopardy.t -> Jeopardy.category_name -> int -> string-> result

(** [bet st n] is [r] when a player bets in the final round of jeopardy. The 
    result is a new final bet amount [n]. *)
val bet : t -> int -> result

(** [final_asnwer jeop st] is [r] if attempting to answer the final question
    Depending on whether [ans] is correct, the score will increase by the final bet.*)
val final_answer: Jeopardy.t -> t -> string -> result