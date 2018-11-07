(** [category_status] represents a category's name and the levels unplayed *)
type category_status 

(** [current_player] represent a player, either player [One] or [Two]. *)
type current_player = | One | Two

(** [t] represents a 2-player game state with current categories 
    [categories_left] and their statuses represented by [categories]. 
    [player1_score] represents player1's current total score, [player2_score] 
    represents player2's current total score, [current_player] represents
    the current player (either One or Two), [player1_passes] and 
    [player2_passes] represents player One and Two's hints remaining hints left.
    [board] represents the 
    current game board. *)
type t

(** [init_state jeop] is the initial state of the 2-player game when playing 
    [jeop]. Both players have a starting score of 0 *)
val init_state : Jeopardy.t -> t

val switch : t -> t

(* [current_player_used_skip st] returns true if the current player in 
    [st] has used their skip, false otherwise. *)
val current_player_used_skip : t -> bool

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

(** [get_player1_bet st] gets the final bet of player 1 in [st]*)
val get_player1_bet: t -> int

(** [get_player2_bet st] gets the final bet of player 2 in [st]*)
val get_player2_bet: t -> int

(** [player1_double_used st] is whether player1 has used the double superpower
    in the game state [st]. *)
val player1_double_used: t -> bool

(** [player2_double_used st] is whether player2 has used the double superpower
    in the game state [st]. *)
val player2_double_used: t -> bool

(** [has_played_final st] is whether the final jeopardy round has been played 
    in [st].*)
val has_played_final: t -> bool

(** [player1_passes_left st] gets the current number of passes left of 
    player 1 from [st]. *)
val player1_passes_left: t -> int

(** [player2_passes_left st] gets the current number of passes left of 
    player 2 from [st]. *)
val player2_passes_left: t -> int
(** [current_category_levels st cat] returns the current levels still unplayed
    in category [cat] in [st]. *)
val current_category_levels : t -> Jeopardy.category_name -> int list

type result = Legal of t | Illegal

(** [play cat lev jeop st] is [r] if attempting to select the question in
    categoty [cat] and level [lev] in jeopardy [jeop]. If [cat] or [lev]
    doesn't exist in [jeop] the result is Illegal, otherwise it is [Legal st']
    where [st'] is the game state after playing the question, with the
    current player switched. *)
val play : Jeopardy.category_name -> int -> Jeopardy.t -> t -> result

(** [answer cat lev jeop st] is [r] if attempting to answer the question
    in [cat] and [lev] in [jeop]. Depending on whether or not [ans] is correct,
    the current player's score either increases or decreases by the score of 
    [lev]. If [lev] or [cat] doesn't exist as an unanswered question, the 
    result is [Illegal]. *)
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

(** [bet st n1 n2] is [r] when a player bets in the final round of jeopardy. 
    The result is a new final bet amount [n1] for player 1 and [n2] for 
    player 2. *)
val bet : t -> int -> int -> result

(** [double st jeop cat lev ans] is [r] if requesting to use the 
    double-or-nothing ability in state [st]. If the current player has already 
    used the ability, then the result is [Illegal], otherwise used_double for
    the current player in [st] is set to true, and the player gets twice 
    the points for the current level [lev] of category [cat] in [jeop] if 
    the answer [ans] is correct and lose twice the points if [ans] is wrong. *)
val double: t -> Jeopardy.t -> Jeopardy.category_name -> int -> string -> result

(** [final_answer jeop st] is [r] if attempting to answer the final question
    Depending on the player and whether answer is correct, the score will 
    increase by the player's final bet*)
val final_answer: Jeopardy.t -> t -> string -> string -> result

(** [skip cat lev ans jeop st] is [r] if requesting to use the skip ability in 
    state [st]. used_skip for the current player in [st] is set to true, and 
    the player gets the points for the current level [lev] of category [cat] in 
    [jeop] if the answer [ans] is correct and lose the points if [ans] is 
    wrong. *)
val skip: Jeopardy.category_name -> int -> string -> Jeopardy.t -> t -> result
