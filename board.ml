(**[valid_length_lengths jeop] is [true] if the number of levels for each 
    category in [jeop] is the same, and [false] otherwise.  *)
let valid_level_lengths jeop =
  let level_list = Jeopardy.all_levels jeop in
  let length = List.length (List.nth level_list 0) in
  let rec check_levels (lst: int list list) =
    match lst with
    | [] -> true
    | h::t -> if (List.length h) <> length then false else check_levels t
  in check_levels level_list

(** [get_max_level_length jeop] is the number of digits of the highest 
    level in [jeop]. *)
let get_max_level_length jeop =
  let cats = Jeopardy.categories jeop in
  let first_cat = Jeopardy.get_category_name (List.nth cats 0) in
  let levels = Jeopardy.levels jeop first_cat in
  let rec get_max_length (lst: int list) (acc: int) =
    match lst with
    | [] -> acc
    | h::t -> let level_length = (String.length (string_of_int h)) in
      if level_length > acc then get_max_length t level_length else
        get_max_length t acc
  in get_max_length levels 0

(** [get_max_cat_length jeop] is the number of letters of the longest 
    category name in [jeop]. *)
let get_max_cat_length jeop =
  let cats = Jeopardy.categories jeop in
  let cat_names = List.map Jeopardy.get_category_name cats in
  let cat_name_strings = List.map Jeopardy.category_name_string  cat_names in
  let rec get_max_length (lst: string list) (acc: int) =
    match lst with
    | [] -> acc
    | h::t -> let cat_length = (String.length h) in    
      if cat_length > acc then get_max_length t cat_length else
        get_max_length t acc in
  get_max_length cat_name_strings 0

(** [max_length jeop] is the maximum number of digits/letters of either the 
    highest level or longest category name in [jeop]. *)
let max_length jeop =
  let lev_length = get_max_level_length jeop in
  let cat_length = get_max_cat_length jeop in
  if lev_length > cat_length then lev_length else cat_length

(** [get_num_columns jeop] is the number of columns to be in the jeopardy board
    from [jeop], i.e. the number of categories.  *)
let get_num_columns jeop =
  let cats = Jeopardy.categories jeop in
  List.length cats

(** [get_num_rows jeop] is the number of rows to be in the jeopardy board from
    [jeop], i.e. the number of levels + 1 for the category row.  *)  
let get_num_rows jeop =
  let cats = Jeopardy.categories jeop in
  let first_cat = Jeopardy.get_category_name (List.nth cats 0) in
  let levels = Jeopardy.levels jeop first_cat in
  (List.length levels) + 1

(** [get_box i acc] is the string with integer [i] number of spaces 
    concatenated to the string [acc]. *)
let rec get_box (i: int) acc =
  match i with
  | 0 -> acc
  | _ -> get_box (i-1) (acc ^ " ")

(** [cat_box n cat] represents a box in a jeopardy game with [|] on both sides 
    and integer [n] characters in between, including the string [cat] that 
    is left-centered.
    Example: [cat_box 6 "hi"] = [|hi    |]. *)
let cat_box (n : int) (cat: string) =
  ("|" ^ (cat) ^ (get_box (n - (String.length cat)) "") ^ "|")

(** [make_cat_row jeop] represents the top row in a jeopardy game containing
    categories in [jeop]. *)
let make_cat_row (jeop: Jeopardy.t) =
  let box_length = max_length jeop in
  let cats = Jeopardy.categories jeop in
  let cat_names = List.map Jeopardy.get_category_name cats in
  let cat_name_strings = List.map Jeopardy.category_name_string cat_names in
  let rec board_cat (cats: string list) acc =
    match cats with 
    | [] -> acc
    | h::t -> board_cat t (acc ^ cat_box box_length h)
  in "|" ^ board_cat cat_name_strings "" ^ "|" ^ "\n"

(** [board_box n lev] represents a box in a jeopardy game with [|] on both sides 
    and integer [n] characters in between, including the integer [lev] that 
    is left-centered.
    Example: [board_box 7 300] = [|300    |]. *)
let board_box (n : int) (lev: int) =
  if lev = 0 then "|" ^ (get_box n "") ^ "|"
  else 
    let lev_string = string_of_int lev in
    ("|" ^ (lev_string) ^ (get_box (n - (String.length lev_string)) "") ^ "|")

(** [make_row n row levs] represents a row in a jeopardy game containing 
    boxes of length [n], which contain the integer [row]th level in each 
    int list in int list list [levs]. 
    Example: [make_row 5 1 [[[1, 2, 3]; [4, 5, 6]; [7, 8, 9]]] =
    [|2    ||5    ||8    |]*)
let rec make_row (n: int) (row: int) (levs : int list list) =
  let row_levels = 
    List.fold_right (fun lst acc -> (List.nth lst row)::acc) levs [] in
  List.fold_left (fun acc x -> acc ^ (board_box n x)) "" (List.rev row_levels) 

(** [game_board jeop levs] is the game board representing categories in [jeop]
    and the levels in each int list in int list list [levs]. *)
let game_board jeop (levs: int list list) =
  let box_length = max_length jeop in
  let rows = get_num_rows jeop in
  let columns = get_num_columns jeop in
  let rec make_board (r : int) (c: int) (pos: int) (acc: string) : string =
    match r with
    | 0 -> acc
    | _ -> (let row = ("|" ^ (make_row box_length pos levs) ^ "|") in
            make_board (r-1) c (pos+1) (acc ^ row ^ "\n"))
  in let levs_board = make_board (rows-1) columns 0 "" in
  (make_cat_row jeop) ^ levs_board
