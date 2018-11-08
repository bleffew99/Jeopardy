open Jeopardy

(** [category_status] represents a category's name and the levels unplayed *)
type category_status = {
  name: category_name;
  levels_left: int list;
}

(** [current_player] represent a player, either player [One] or [Two]. *)
type current_player = | One | Two

(** [t] represents a 2-player game state with current categories 
    [categories_left] and their statuses represented by [categories]. 
    [player1_score] represents player1's current total score, [player2_score] 
    represents player2's current total score, [current_player] represents
    the current player (either One or Two), [player1_passes] and 
    [player2_passes] represents player One and Two's hints remaining hints left.
    [played_final] is whether the final round has been plaeyd. [board] 
    represents the current game board. *)
type t = {
  categories : category_status list;
  categories_left : category_name list;
  player1_score : int;
  player2_score : int;
  current_player : current_player;
  player1_passes : int;
  player2_passes : int;
  player1_bet : int;
  player2_bet : int;
  player1_used_double : bool;
  player2_used_double : bool;
  player1_used_skip : bool;
  player2_used_skip : bool;
  played_final : bool;
  board : string
}

let switch st =
  {categories = st.categories; 
   categories_left = st.categories_left; 
   player1_score = st.player1_score;
   player2_score = st.player2_score;
   current_player = if st.current_player = One then Two else One; 
   player1_passes = st.player1_passes;
   player2_passes = st.player2_passes; 
   player1_bet = st.player1_bet;
   player2_bet = st.player2_bet;
   player1_used_double = st.player1_used_double;
   player2_used_double = st.player2_used_double;
   player1_used_skip = if st.current_player = One then true else st.player1_used_skip;
   player2_used_skip = if st.current_player = Two then true else st.player2_used_skip;                            
   played_final = false;                                                       
   board = st.board}

(** [parse_levels] is the list of level scores from [levels]. *)
let rec parse_levels (levels: Jeopardy.level list): int list =
  let rec helper levels acc =
    match levels with
    | [] -> acc
    | h::t -> helper t ((score h) :: acc)
  in List.rev (helper levels [])

(** [parse_categories] is the initial list of category statuses present in 
    [cats], a Jeopardy.category list. *)  
let rec parse_categories (cats: Jeopardy.category list) 
    acc : category_status list = 
  match cats with
  | [] -> acc
  | h::t -> parse_categories t 
              (({name = get_category_name h; 
                 levels_left = (parse_levels (get_category_levels h))})::acc)

(** [init_state jeop] is the initial state of the 2-player game when playing 
    [jeop]. Both players have a starting score of 0 *)
let init_state (jeop: Jeopardy.t) : t = 
  let cats = Jeopardy.categories jeop in
  let cat_names = List.map Jeopardy.get_category_name cats in
  let level_list = List.map (Jeopardy.levels jeop) cat_names in
  {categories = parse_categories (categories jeop) []; 
   categories_left = Jeopardy.categories_list jeop;
   player1_score = 0; 
   player2_score = 0;
   current_player = One;
   player1_passes = 3;
   player2_passes = 3;
   player1_bet = 0;
   player2_bet = 0;
   player1_used_double = false;
   player2_used_double = false;
   player1_used_skip = false;
   player2_used_skip = false;       
   played_final = false;
   board = Board.game_board jeop level_list}

(* [current_player_used_skip st] returns true if the current player in 
    [st] has used their skip, false otherwise. *)
let current_player_used_skip (st: t) = 
  if st.current_player = One then st.player1_used_skip else st.player2_used_skip

(** [current_categories st] gets the categories left field from [st]. *)  
let current_categories (st: t) =
  st.categories_left

(** [current_player1_score st] gets the current score of player 1 from [st]. *)
let current_player1_score st =
  st.player1_score

(** [current_player2_score st] gets the current score of player 2 from [st]. *)
let current_player2_score st =
  st.player2_score

(** [player1_passes_left st] gets the current number of passes left of 
    player 1 from [st]. *)
let player1_passes_left st =
  st.player1_passes

(** [player2_passes_left st] gets the current number of passes left of 
    player 2 from [st]. *)
let player2_passes_left st =
  st.player2_passes

(** [current_board st] gets the current game board from [st]. *)
let current_board st =
  st.board

(** [get_current_player st] gets the current player in [st]. *)
let get_current_player st =
  st.current_player

(** [get_player1_bet st] gets the final bet of player 1 in [st]*)
let get_player1_bet st =
  st.player1_bet

(** [get_player2_bet st] gets the final bet of player 2 in [st]*)
let get_player2_bet st =
  st.player2_bet

(** [player1_double_used st] is whether player1 has used the double superpower
    in the game state [st]. *)
let player1_double_used st = 
  st.player1_used_double

(** [player2_double_used st] is whether player2 has used the double superpower
    in the game state [st]. *)
let player2_double_used st =
  st.player2_used_double

(** [has_played_final st] is whether the final jeopardy round has been played 
    in [st]. *)
let has_played_final st = 
  st.played_final

(** [current_category_levels st cat] returns the current levels still unplayed
    in category [cat] in [st]. *)
let current_category_levels st (cat: category_name) : int list =
  let rec find_category (cats: category_status list) cat = 
    match cats with
    | [] -> raise (UnknownCategory cat)
    | h::t -> 
      if String.lowercase_ascii (Jeopardy.category_name_string h.name) = 
         String.lowercase_ascii (Jeopardy.category_name_string cat) 
      then h.levels_left else find_category t cat
  in
  find_category st.categories cat

type result = Legal of t | Illegal

(** [remove_category lst cat acc] is category_name list [lst] with 
    category_name [cat] removed. *)
let remove_category (lst : category_name list) (cat : category_name) = 
  let rec helper l acc =
    match l with
    | [] -> acc
    | h::t -> 
      if (String.lowercase_ascii (Jeopardy.category_name_string h) = 
          String.lowercase_ascii (Jeopardy.category_name_string cat)) 
      then helper t acc
      else helper t (h::acc) 
  in List.rev (helper lst [])

(** [remove_level lst lev] is int list [lst] with int [lev] removed. *)
let rec remove_level (lst : int list) (lev : int) =
  let rec helper lst lev acc =
    match lst with
    | [] -> acc
    | h::t -> if h = lev then helper t lev acc
      else helper t lev (h::acc)
  in List.rev (helper lst lev [])

(* [remove_category_level lst cat lev] is category_status list [lst] with the 
   level [lev] of the category with category_name [cat] removed.*)
let rec remove_category_level (lst : category_status list) 
    (cat : category_name) (lev : int) =
  let rec helper lst' acc = 
    match lst' with 
    | [] -> acc
    | h::t -> 
      if (String.lowercase_ascii (Jeopardy.category_name_string h.name) = 
          String.lowercase_ascii (Jeopardy.category_name_string cat))
      then helper t (({name = h.name; 
                       levels_left = remove_level h.levels_left lev }) :: acc)
      else helper t (h::acc) 
  in List.rev (helper lst [])

(** [all_levels_left cats_lst] returns a list of all the levels left in each 
    category in category_status list [cats_lst]. *)
let all_levels_left (cats_lst: category_status list) =
  let rec get_levels (cats: category_status list) (acc : int list list) =
    match cats with
    | [] -> acc
    | h::t -> get_levels t (h.levels_left :: acc)
  in List.rev (get_levels cats_lst [])

(** [new_board jeop cats] returns the new board for [jeop] with only remaining
    categories in category_status list [cats]. *)
let new_board (jeop: Jeopardy.t) (cats: category_status list) : string =
  let rec level_diffs 
      (full_levs: int list list) (curr_levs: int list list) acc =
    match full_levs, curr_levs with
    | h::t, h'::t' -> (let rec diffs (lst1 : int list) (lst2 : int list) acc =
                         match lst1 with
                         | [] -> acc
                         | h::t -> 
                           let acc' = if List.mem h lst2 then h::acc else 0::acc 
                           in diffs t lst2 acc' in
                       level_diffs t t' (((List.rev (diffs h h' []))):: acc))
    | _ , _ -> acc
  in 
  let new_levs =
    List.rev (level_diffs (Jeopardy.all_levels jeop) (all_levels_left cats) []) 
  in Board.game_board jeop new_levs

(** [play cat lev jeop st] is [r] if attempting to select the question in
    categoty [cat] and level [lev] in jeopardy [jeop]. If [cat] or [lev]
    doesn't exist in [jeop] the result is Illegal, otherwise it is [Legal st']
    where [st'] is the game state after playing the question, with the
    current player switched. *)
let play cat lev (jeop: Jeopardy.t) st = 
  if not 
      (List.mem (String.lowercase_ascii (Jeopardy.category_name_string cat)) 
         (List.map String.lowercase_ascii 
            (List.map Jeopardy.category_name_string st.categories_left))) 
  then Illegal 
  else let levels = current_category_levels st cat in
    if not (List.mem lev levels) then Illegal 
    else if List.length levels = 1 
    then Legal {categories = remove_category_level st.categories cat lev; 
                categories_left = remove_category st.categories_left cat; 
                player1_score = st.player1_score;
                player2_score = st.player2_score;
                current_player = if st.current_player = One then Two else One; 
                player1_passes = st.player1_passes;
                player2_passes = st.player2_passes; 
                player1_bet = st.player1_bet;
                player2_bet = st.player2_bet;
                player1_used_double = st.player1_used_double;
                player2_used_double = st.player2_used_double;
                player1_used_skip = st.player1_used_skip;
                player2_used_skip = st.player2_used_skip;                              
                played_final = false;                                                       
                board = new_board jeop 
                    (remove_category_level st.categories cat lev)}
    else Legal {categories = remove_category_level st.categories cat lev; 
                categories_left = st.categories_left; 
                player1_score = st.player1_score;
                player2_score = st.player2_score;
                current_player = if st.current_player = One then Two else One;
                player1_passes = st.player1_passes;
                player2_passes = st.player2_passes;   
                player1_bet = st.player1_bet;
                player2_bet = st.player2_bet;
                player1_used_double = st.player1_used_double;
                player2_used_double = st.player2_used_double;
                player1_used_skip = st.player1_used_skip;
                player2_used_skip = st.player2_used_skip;  
                played_final = false;
                board = new_board jeop 
                    (remove_category_level st.categories cat lev)}

(** [answer cat lev jeop st] is [r] if attempting to answer the question
    in [cat] and [lev] in [jeop]. Depending on whether or not [ans] is correct,
    the current player's score either increases or decreases by the score of 
    [lev]. If [lev] or [cat] doesn't exist as an unanswered question, the 
    result is [Illegal]. *)
let answer (cat : Jeopardy.category_name) lev (ans: string) 
    (jeop: Jeopardy.t) (st: t) =   
  try (let corrects = answers jeop cat lev in 
       (if List.mem ans corrects then 
          Legal {categories = st.categories; 
                 categories_left = st.categories_left;
                 player1_score = if st.current_player = One then
                     st.player1_score + lev else st.player1_score;
                 player2_score = if st.current_player = Two then
                     st.player2_score + lev else st.player2_score;
                 current_player = st.current_player;
                 player1_passes = st.player1_passes;
                 player2_passes = st.player2_passes;   
                 player1_bet = st.player1_bet;
                 player2_bet = st.player2_bet;
                 player1_used_double = st.player1_used_double;
                 player2_used_double = st.player2_used_double;
                 player1_used_skip = st.player1_used_skip;
                 player2_used_skip = st.player2_used_skip;  
                 played_final = false;
                 board = st.board}
        else Legal {categories = st.categories; 
                    categories_left = st.categories_left;
                    player1_score = if st.current_player = One then
                        st.player1_score - lev else st.player1_score;
                    player2_score = if st.current_player = Two then
                        st.player2_score - lev else st.player2_score;
                    current_player = st.current_player;
                    player1_passes = st.player1_passes;
                    player2_passes = st.player2_passes;  
                    player1_bet = st.player1_bet;
                    player2_bet = st.player2_bet; 
                    player1_used_double = st.player1_used_double;
                    player2_used_double = st.player2_used_double;
                    player1_used_skip = st.player1_used_skip;
                    player2_used_skip = st.player2_used_skip;  
                    played_final = false;
                    board = st.board}))
  with 
  | NoAnswersProvided -> Illegal 
  | UnknownCategory cat -> Illegal

(** [hint cat lev jeop st] is [r] if requesting a hint for question [lev] in
    category [cat]. If [lev] or [cat] doesn't exist the result is [Illegal],
    otherwise the player is deducted 100 points for asking for a hint.
    Hint does NO printing. *)
let hint (cat: Jeopardy.category_name) (lev:int) (jeop: Jeopardy.t) st =
  try (let _ = Jeopardy.hint jeop cat lev in
       Legal {categories = st.categories;
              categories_left = st.categories_left;
              player1_score = if st.current_player = One then 
                  st.player1_score - 100 else st.player1_score;
              player2_score = if st.current_player = Two then 
                  st.player2_score - 100 else st.player2_score;
              current_player = st.current_player;
              player1_passes = st.player1_passes;
              player2_passes = st.player2_passes;
              player1_bet = st.player1_bet;
              player2_bet = st.player2_bet;
              player1_used_double = st.player1_used_double;
              player2_used_double = st.player2_used_double;
              player1_used_skip = st.player1_used_skip;
              player2_used_skip = st.player2_used_skip;  
              played_final = false;
              board = st.board})
  with
  | UnknownLevel lev -> Illegal
  | UnknownCategory cat -> Illegal

(** [pass st] is [r] if requesting a pass in state [st]. If the number of passes
    left is 0, then the result is [Illegal] otherwise the player is deducted
    1 pass for asking for a pass. *)  
let pass st =
  if st.current_player = One then
    if st.player1_passes = 0 then Illegal 
    else Legal {categories = st.categories; 
                categories_left = st.categories_left; 
                player1_score = st.player1_score;
                player2_score = st.player2_score;
                current_player = st.current_player;
                player1_passes = st.player1_passes -1;
                player2_passes = st.player2_passes;   
                player1_bet = st.player1_bet;
                player2_bet = st.player2_bet;
                player1_used_double = st.player1_used_double;
                player2_used_double = st.player2_used_double;
                player1_used_skip = st.player1_used_skip;
                player2_used_skip = st.player2_used_skip;  
                played_final = false;
                board = st.board}
  else
  if st.player2_passes = 0 then Illegal 
  else Legal {categories = st.categories; 
              categories_left = st.categories_left; 
              player1_score = st.player1_score;
              player2_score = st.player2_score;
              current_player = st.current_player;
              player1_passes = st.player1_passes;
              player2_passes = st.player2_passes -1; 
              player1_bet = st.player1_bet;
              player2_bet = st.player2_bet;
              player1_used_double = st.player1_used_double;
              player2_used_double = st.player2_used_double;
              player1_used_skip = st.player1_used_skip;
              player2_used_skip = st.player2_used_skip;  
              played_final = false;  
              board = st.board}

(** [bet st n1 n2] is [r] when a player bets in the final round of jeopardy. 
    The result is a new final bet amount [n1] for player 1 and [n2] for 
    player 2. *)
let bet st (n1: int) (n2: int) =
  Legal {categories = st.categories; 
         categories_left = st.categories_left; 
         player1_score = st.player1_score;
         player2_score = st.player2_score;
         current_player = st.current_player;
         player1_passes = st.player1_passes;
         player2_passes = st.player2_passes; 
         player1_bet = n1;
         player2_bet = n2;
         player1_used_double = st.player1_used_double;
         player2_used_double = st.player2_used_double;
         player1_used_skip = st.player1_used_skip;
         player2_used_skip = st.player2_used_skip;  
         played_final = false;  
         board = st.board}

(** [double st jeop cat lev ans] is [r] if requesting to use the 
    double-or-nothing ability in state [st]. If the current player has already 
    used the ability, then the result is [Illegal], otherwise used_double for
    the current player in [st] is set to true, and the player gets twice 
    the points for the current level [lev] of category [cat] in [jeop] if 
    the answer [ans] is correct and lose twice the points if [ans] is wrong. *)
let double (st : t) (jeop: Jeopardy.t) (cat: Jeopardy.category_name) 
    (lev: int) (ans: string) = 
  if st.current_player = One then 
    if st.player1_used_double = false then
      (let corrects = answers jeop cat lev in 
       if List.mem ans corrects then 
         Legal {categories = st.categories; 
                categories_left = st.categories_left;
                player1_score = st.player1_score + (lev * 2);
                player2_score = st.player2_score;
                current_player = st.current_player;
                player1_passes = st.player1_passes;
                player2_passes = st.player2_passes; 
                player1_bet = st.player1_bet;
                player2_bet = st.player2_bet;
                player1_used_double = true;
                player2_used_double = st.player2_used_double;
                player1_used_skip = st.player1_used_skip;
                player2_used_skip = st.player2_used_skip;  
                played_final = st.played_final;  
                board = st.board}
       else Legal {categories = st.categories; 
                   categories_left = st.categories_left;
                   player1_score = st.player1_score - (lev * 2);
                   player2_score = st.player2_score;
                   current_player = st.current_player;
                   player1_passes = st.player1_passes;
                   player2_passes = st.player2_passes; 
                   player1_bet = st.player1_bet;
                   player2_bet = st.player2_bet;
                   player1_used_double = true;
                   player2_used_double = st.player2_used_double;
                   player1_used_skip = st.player1_used_skip;
                   player2_used_skip = st.player2_used_skip;  
                   played_final = st.played_final;  
                   board = st.board})
    else
      Illegal
  else 
    (if st.player2_used_double = false then
       (let corrects = answers jeop cat lev in 
        if List.mem ans corrects then 
          Legal {categories = st.categories; 
                 categories_left = st.categories_left;
                 player1_score = st.player1_score;
                 player2_score = st.player2_score + (lev * 2);
                 current_player = st.current_player;
                 player1_passes = st.player1_passes;
                 player2_passes = st.player2_passes; 
                 player1_bet = st.player1_bet;
                 player2_bet = st.player2_bet;
                 player1_used_double = st.player1_used_double;
                 player2_used_double = true;
                 player1_used_skip = st.player1_used_skip;
                 player2_used_skip = st.player2_used_skip;  
                 played_final = st.played_final;  
                 board = st.board}
        else Legal {categories = st.categories; 
                    categories_left = st.categories_left;
                    player1_score = st.player1_score;
                    player2_score = st.player2_score - (lev * 2);
                    current_player = st.current_player;
                    player1_passes = st.player1_passes;
                    player2_passes = st.player2_passes; 
                    player1_bet = st.player1_bet;
                    player2_bet = st.player2_bet;
                    player1_used_double = st.player1_used_double;
                    player2_used_double = true;
                    player1_used_skip = st.player1_used_skip;
                    player2_used_skip = st.player2_used_skip;  
                    played_final = st.played_final;  
                    board = st.board})
     else
       Illegal)

(** [final_answer jeop st] is [r] if attempting to answer the final question
    Depending on the player and whether answer is correct, the score will 
    increase by the player's final bet*)
let final_answer jeop st (ans1: string) (ans2: string) =
  if List.mem ans1 (Jeopardy.final_jeopardy_answers jeop) then
    (print_endline ("Player 1, you are correct!");
     if List.mem ans2 (Jeopardy.final_jeopardy_answers jeop) then
       (print_endline ("Player 2, you are correct!");
        (Legal {categories = st.categories;
                categories_left = st.categories_left;
                player1_score = st.player1_score + (get_player1_bet st);
                player2_score = st.player2_score + (get_player2_bet st);
                current_player = st.current_player;
                player1_passes = st.player1_passes;
                player2_passes = st.player2_passes; 
                player1_bet = st.player1_bet;
                player2_bet = st.player2_bet;
                player1_used_double = st.player1_used_double;
                player2_used_double = st.player2_used_double;
                player1_used_skip = st.player1_used_skip;
                player2_used_skip = st.player2_used_skip;                  
                played_final = true;
                board = st.board}))
     else 
       (print_endline ("Player 2, sorry but you are wrong!");
        (Legal {categories = st.categories;
                categories_left = st.categories_left;
                player1_score = st.player1_score + (get_player1_bet st);
                player2_score = st.player2_score - (get_player2_bet st);
                current_player = st.current_player;
                player1_passes = st.player1_passes;
                player2_passes = st.player2_passes; 
                player1_bet = st.player1_bet;
                player2_bet = st.player2_bet;
                player1_used_double = st.player1_used_double;
                player2_used_double = st.player2_used_double;
                player1_used_skip = st.player1_used_skip;
                player2_used_skip = st.player2_used_skip;  
                played_final = true;
                board = st.board})))
  else 
    (print_endline ("Player 1, sorry but you are wrong!");
     if List.mem ans2 (Jeopardy.final_jeopardy_answers jeop) then
       (print_endline ("Player 2, you are correct!");
        Legal {categories = st.categories;
               categories_left = st.categories_left;
               player1_score = st.player1_score - (get_player1_bet st);
               player2_score = st.player2_score + (get_player2_bet st);
               current_player = st.current_player;
               player1_passes = st.player1_passes;
               player2_passes = st.player2_passes; 
               player1_bet = st.player1_bet;
               player2_bet = st.player2_bet;
               player1_used_double = st.player1_used_double;
               player2_used_double = st.player2_used_double;
               player1_used_skip = st.player1_used_skip;
               player2_used_skip = st.player2_used_skip;  
               played_final = true;
               board = st.board} )
     else 
       (print_endline ("Player 2, sorry but you are wrong, too!");
        (print_string "The correct answer was:");
        (print_endline (List.nth (Jeopardy.final_jeopardy_answers jeop) 0));
        Legal {categories = st.categories;
               categories_left = st.categories_left;
               player1_score = st.player1_score - (get_player1_bet st);
               player2_score = st.player2_score - (get_player2_bet st);
               current_player = st.current_player;
               player1_passes = st.player1_passes;
               player2_passes = st.player2_passes; 
               player1_bet = st.player1_bet;
               player2_bet = st.player2_bet;
               player1_used_double = st.player1_used_double;
               player2_used_double = st.player2_used_double;
               player1_used_skip = st.player1_used_skip;
               player2_used_skip = st.player2_used_skip;  
               played_final = true;
               board = st.board} ))

(** [skip cat lev ans jeop st] is [r] if requesting to use the skip ability in 
    state [st]. used_skip for the current player in [st] is set to true, and 
    the player gets the points for the current level [lev] of category [cat] in 
    [jeop] if the answer [ans] is correct and lose the points if [ans] is 
    wrong. *)
let skip (cat : Jeopardy.category_name) lev (ans: string) 
    (jeop: Jeopardy.t) (st: t) =   
  try (let corrects = answers jeop cat lev in 
       (if List.mem ans corrects then 
          Legal {categories = st.categories; 
                 categories_left = st.categories_left;
                 player1_score = if st.current_player = One then
                     st.player1_score + lev else st.player1_score;
                 player2_score = if st.current_player = Two then
                     st.player2_score + lev else st.player2_score;
                 current_player = if st.current_player = One then Two else One;
                 player1_passes = st.player1_passes;
                 player2_passes = st.player2_passes;   
                 player1_bet = st.player1_bet;
                 player2_bet = st.player2_bet;
                 player1_used_double = st.player1_used_double;
                 player2_used_double = st.player2_used_double;
                 player1_used_skip = 
                   if st.current_player = One then true 
                   else st.player1_used_skip;
                 player2_used_skip = 
                   if st.current_player = Two then true 
                   else st.player2_used_skip;  
                 played_final = false;
                 board = st.board}
        else Legal {categories = st.categories; 
                    categories_left = st.categories_left;
                    player1_score = if st.current_player = One then
                        st.player1_score - lev else st.player1_score;
                    player2_score = if st.current_player = Two then
                        st.player2_score - lev else st.player2_score;
                    current_player = 
                      if st.current_player = One then Two else One;
                    player1_passes = st.player1_passes;
                    player2_passes = st.player2_passes;  
                    player1_bet = st.player1_bet;
                    player2_bet = st.player2_bet; 
                    player1_used_double = st.player1_used_double;
                    player2_used_double = st.player2_used_double;
                    player1_used_skip = 
                      if st.current_player = One then true 
                      else st.player1_used_skip;
                    player2_used_skip =
                      if st.current_player = Two then true 
                      else st.player2_used_skip; 
                    played_final = false;
                    board = st.board}))
  with 
  | NoAnswersProvided -> Illegal 
  | UnknownCategory cat -> Illegal