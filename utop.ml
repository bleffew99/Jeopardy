let valid_level_lengths jeop =
  let cats = categories jeop in
  let cat_names = List.map get_category_name cats in
  let level_list = List.map (levels jeop) cat_names in
  let length = List.length (List.nth level_list 0) in
  let rec check_levels (lst: int list list) =
    match lst with
    | [] -> true
    | h::t -> if (List.length h) <> length then false else check_levels t
  in check_levels level_list

(** [get_max_level_length jeop] is the number of digits of the highest 
    level in [jeop] *)
let get_max_level_length jeop =
  let cats = categories jeop in
  let first_cat = get_category_name (List.nth cats 0) in
  let levels = levels jeop first_cat in
  let rec get_max_length (lst: int list) (acc: int) =
    match lst with
    | [] -> acc
    | h::t -> let level_length = (String.length (string_of_int h)) in
      if level_length > acc then get_max_length t level_length else
        get_max_length t acc
  in get_max_length levels 0

(** [get_max_cat_length jeop] is the number of letters of the longest 
    category name in [jeop]*)
let get_max_cat_length jeop =
  let cats = categories jeop in
  let cat_names = List.map get_category_name cats in
  let cat_name_strings = List.map category_name_string  cat_names in
  let rec get_max_length (lst: string list) (acc: int) =
    match lst with
    | [] -> acc
    | h::t -> let cat_length = (String.length h) in    
      if cat_length > acc then get_max_length t cat_length else
        get_max_length t acc in
  get_max_length cat_name_strings 0

(** [max_length jeop] is the maximum number of digits/strings of either the 
    highest level or longest category name in [jeop]*)
let max_length jeop =
  let lev_length = get_max_level_length jeop in
  let cat_length = get_max_cat_length jeop in
  if lev_length > cat_length then lev_length else cat_length

(** [get_num_columns jeop] is the number of columns to be in the jeopardy board
    from [jeop], ie the number of categories.  *)
let get_num_columns jeop =
  let cats = categories jeop in
  List.length cats

(** [get_num_rows jeop] is the number of rows to be in the jeopardy board from
    [jeop], ie the number of levels + 1 for the category row.  *)  
let get_num_rows jeop =
  let cats = categories jeop in
  let first_cat = get_category_name (List.nth cats 0) in
  let levels = levels jeop first_cat in
  (List.length levels) + 1


let rec get_box (i: int) acc =
  match i with
  | 0 -> acc
  | _ -> get_box (i-1) (acc ^ " ")

(*[n] is box length*)
let cat_box (n : int) (cat: string) =
  ("|" ^ (cat) ^ (get_box (n - (String.length cat)) "") ^ "|")
(*
let rec make_columns col (n: int) (acc : string) : string = 
  match col with
  | 0 -> acc
  | _ -> make_columns (col-1) n (acc ^ (board_box n))
*)

let make_cat_row (jeop: t) =
  let box_length = max_length jeop in
  let cats = categories jeop in
  let cat_names = List.map get_category_name cats in
  let cat_name_strings = List.map category_name_string  cat_names in
  let rec board_cat (cats: string list) acc =
    match cats with 
    | [] -> acc
    | h::t -> board_cat t (acc ^ cat_box box_length h)
  in "|" ^ board_cat cat_name_strings "" ^ "|" ^ "\n"

let board_box (n : int) (lev: int) =
  if lev = 0 then "|" ^ (get_box n "") ^ "|"
  else 
    let lev_string = string_of_int lev in
    ("|" ^ (lev_string) ^ (get_box (n - (String.length lev_string)) "") ^ "|")

(* col is columns in a row, n is box length, row is which row of levels, 
   take in rows of levels and outputs 1*)
let rec make_row (n: int) (row: int) (levs : int list list) =
  let row_levels = 
    List.fold_right (fun lst acc -> (List.nth lst row)::acc) levs [] in
  List.fold_left (fun acc x -> acc ^ (board_box n x)) "" row_levels  

(** pos starts at 0*)
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
