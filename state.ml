open Jeopardy

(** [category_status] represents a category's name and the levels unplayed *)
type category_status = {
  name: category_name;
  levels_left: int list;
}

(** [t] represents a game state with current categories [categories_left] and
    their statuses represented by [categories]. [score] represents the player's
    current total score. *)
type t = {
  categories : category_status list;
  categories_left : category_name list;
  score : int;
}

(** [parse_levels] is the list of level scores from [levels]*)
let rec parse_levels (levels: Jeopardy.level list): int list =
  let rec helper levels acc =
    match levels with
    | [] -> acc
    | h::t -> helper t ((score h) :: acc)
  in List.rev (helper levels [])

(** [parse_categories] is the initial list of category statuses present in 
    [cats], a Jeopardy.category list *)  
let rec parse_categories (cats: Jeopardy.category list) 
    acc : category_status list = 
  match cats with
  | [] -> acc
  | h::t -> parse_categories t 
              (({name = get_category_name h; 
                 levels_left = (parse_levels (get_category_levels h))})::acc)

(** [init_state jeop] is the initial state of the game when playing [jeop].
    They have a starting score of 0 *)
let init_state (jeop: Jeopardy.t) : t = 
  {categories = parse_categories (categories jeop) []; 
   categories_left = Jeopardy.categories_list jeop;
   score = 0 }

(** [current_categories st] gets the categories left field from [st]. *)  
let current_categories (st: t) =
  st.categories_left

(** [current_score st] gets the current score from [st]. *)
let current_score st =
  st.score

(** [current_category_levels st cat] returns the current levels still unplayed
    in category [cat] *)
let current_category_levels st (cat: category_name) : int list =
  let rec find_category (cats: category_status list) cat = 
    match cats with
    | [] -> raise (UnknownCategory cat)
    | h::t -> if (h.name = cat) then h.levels_left else find_category t cat
  in
  find_category st.categories cat

(** [levels_to_string levs] returns the scores in list [levs] in a list, with
    the scores seperated by commas. *)
let levels_to_string (levs: int list) : string =
  let rec levels levs acc =
    match levs with
    | [] -> acc
    | h::m::t -> levels (m::t) (acc ^ (string_of_int h) ^ ",")
    | h::t -> levels t (acc ^ (string_of_int h))
  in levels levs ""

(** [current_category_levels_to_string st] returns a string containing each 
    category in categories_left of [st] followed by the levels left in that 
    category. *)
let current_category_levels_to_string st : string =
  List.fold_right 
    (fun x acc -> ((Jeopardy.category_name_string x)) ^ ": " ^ 
                  (levels_to_string (current_category_levels st x)) 
                  ^ "\n" ^ acc)
    (st.categories_left) ""

type result = Legal of t | Illegal

(** [remove_category lst cat acc] is [lst] but with [cat] removed. *)
let rec remove_category (lst : category_name list) (cat : category_name) acc = 
  match lst with
  | [] -> acc
  | h::t -> if h = cat then remove_category t cat acc
    else remove_category t cat (h::acc)  

(** [remove_level lst lev acc] is [lst] but with [lev] removed. *)
let rec remove_level (lst : int list) (lev : int) =
  let rec helper lst lev acc =
    match lst with
    | [] -> acc
    | h::t -> if h = lev then helper t lev acc
      else helper t lev (h::acc)
  in List.rev (helper lst lev [])

(* [remove_category_level lst cat lev acc] is lst but with the question with
    [cat] and [lev] removed.*)
let rec remove_category_level (lst : category_status list) 
    (cat : category_name) (lev : int) acc =
  match lst with 
  | [] -> acc
  | h::t -> 
    if (h.name = cat) 
    then remove_category_level t cat lev 
        (({name = h.name; 
           levels_left = remove_level h.levels_left lev }) :: acc)
    else remove_category_level t cat lev (h::acc)                    

(** [play cat lev jeop st] is [r] if attempting to select the question in
    categoty [cat] and level [lev] in jeopardy [jeop]. If [cat] or [lev]
    don't exist in [jeop] the result is Illegal, otherwise it is [Legal st']
    where [st'] is the players state after asking the question. *)
let play cat lev (jeop: Jeopardy.t) st = 
  if not (List.mem cat st.categories_left) then Illegal 
  else let levels = current_category_levels st cat in
    if not (List.mem lev levels) then Illegal 
    else if List.length levels = 1 
    then Legal {categories = remove_category_level st.categories cat lev []; 
                categories_left = remove_category st.categories_left cat []; 
                score = st.score}
    else Legal {categories = remove_category_level st.categories cat lev []; 
                categories_left = st.categories_left; score = st.score}


(** [answer cat lev jeop st] is [r] if attempting to answer the question
    in [cat] and [lev] in [jeop]. Depending on whether or not [ans] is correct,
    the player's score either increases or decreases by the score of [lev].
    If lev or cat don't exist as an unanswered question, the result 
    is [Illegal]. *)
let answer (cat : Jeopardy.category_name) lev (ans: string) 
    (jeop: Jeopardy.t) (st: t) =   
  try (let corrects = answers jeop cat lev in 
       if List.mem ans corrects then 
         Legal {categories = st.categories; 
                categories_left = st.categories_left;
                score = st.score + lev}
       else Legal {categories = st.categories; 
                   categories_left = st.categories_left;
                   score = st.score -lev})
  with 
  | NoAnswersProvided -> Illegal 
  | UnknownCategory cat -> Illegal





