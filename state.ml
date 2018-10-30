open Jeopardy

(** [category_status] represents a category's name and the levels unplayed *)
type category_status = {
  name: category_name;
  levels_left: int list;
}

(** [t] represents a game state with current categories [categories_left] and
    their statuses represented by [categories]. [score] represents the player's
    current total score. [board] reprents the current game board. *)
type t = {
  categories : category_status list;
  categories_left : category_name list;
  score : int;
  board : string
}

(** [parse_levels levels] is the list of level scores from [levels]. *)
let rec parse_levels (levels: Jeopardy.level list): int list =
  let rec helper levels acc =
    match levels with
    | [] -> acc
    | h::t -> helper t ((score h) :: acc)
  in List.rev (helper levels [])

(** [parse_categories cats] is the initial list of category statuses present in 
    [cats], a Jeopardy.category list. *)  
let rec parse_categories (cats: Jeopardy.category list) 
    acc : category_status list = 
  match cats with
  | [] -> acc
  | h::t -> parse_categories t 
              (({name = get_category_name h; 
                 levels_left = (parse_levels (get_category_levels h))})::acc)

(** [init_state jeop] is the initial state of the game when playing [jeop].
    The starting score is 0. *)
let init_state (jeop: Jeopardy.t) : t = 
  let cats = Jeopardy.categories jeop in
  let cat_names = List.map Jeopardy.get_category_name cats in
  let level_list = List.map (Jeopardy.levels jeop) cat_names in
  {categories = parse_categories (categories jeop) []; 
   categories_left = Jeopardy.categories_list jeop;
   score = 0; 
   board = Board.game_board jeop level_list}

(** [current_categories st] gets the categories left field from [st]. *)  
let current_categories (st: t) =
  st.categories_left

(** [current_score st] gets the current score from [st]. *)
let current_score st =
  st.score

(** [current_board st] get the current score from [st]*)
let current_board st =
  st.board

(** [current_category_levels st cat] returns the current levels still unplayed
    in category [cat] in [st]. *)
let current_category_levels st (cat: category_name) : int list =
  let rec find_category (cats: category_status list) cat = 
    match cats with
    | [] -> raise (UnknownCategory cat)
    | h::t -> if (h.name = cat) then h.levels_left else find_category t cat
  in
  find_category st.categories cat

type result = Legal of t | Illegal

(** [remove_category lst cat acc] is category_name list [lst] with 
    category_name [cat] removed. *)
let remove_category (lst : category_name list) (cat : category_name) = 
  let rec helper l acc =
    match l with
    | [] -> acc
    | h::t -> if h = cat then helper t acc
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
      if (h.name = cat) 
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
    where [st'] is the player's state after playing the question. *)
let play cat lev (jeop: Jeopardy.t) st = 
  if not (List.mem cat st.categories_left) then Illegal 
  else let levels = current_category_levels st cat in
    if not (List.mem lev levels) then Illegal 
    else if List.length levels = 1 
    then Legal {categories = remove_category_level st.categories cat lev; 
                categories_left = remove_category st.categories_left cat; 
                score = st.score;
                board = new_board jeop 
                    (remove_category_level st.categories cat lev)}
    else Legal {categories = remove_category_level st.categories cat lev; 
                categories_left = st.categories_left; 
                score = st.score;
                board = new_board jeop 
                    (remove_category_level st.categories cat lev)}

(** [answer cat lev jeop st] is [r] if attempting to answer the question
    in [cat] and [lev] in [jeop]. Depending on whether or not [ans] is correct,
    the player's score either increases or decreases by the score of [lev].
    If [lev] or [cat] doesn't exist as an unanswered question, the result 
    is [Illegal]. *)
let answer (cat : Jeopardy.category_name) lev (ans: string) 
    (jeop: Jeopardy.t) (st: t) =   
  try (let corrects = answers jeop cat lev in 
       if List.mem ans corrects then 
         Legal {categories = st.categories; 
                categories_left = st.categories_left;
                score = st.score + lev;
                board = st.board}
       else Legal {categories = st.categories; 
                   categories_left = st.categories_left;
                   score = st.score -lev;
                   board = st.board})
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
              score = st.score - 100;
              board = st.board})
  with
  | UnknownLevel lev -> Illegal
  | UnknownCategory cat -> Illegal




