open Yojson.Basic.Util

(** The type of questions. *)
type question = string

(** The type of category names. *)
type category_name = string

exception UnknownCategory of category_name
exception UnknownLevel of int
exception NoAnswersProvided
exception NoCategoriesProvided

(** [level] represents a level in a category that has a score, a question, 
    a list of possible correct answers, and a hint for the answers. *)
type level = {
  score: int;
  question: question;
  answers: string list;
  hint: string;
}

(** [category] represents a jeopardy category that has a name and a list of
    levels in that category. *)
type category = {
  name: category_name;
  levels: level list;
}

(** [final] represents the final Jeopardy round with a question and a list of 
    possible answers.*)
type final = {
  final_question: question;
  final_answers: string list
}

(** [t] represents a jeopardy game, which includes a list of categories, each
    with levels to choose from. *)
type t = {
  categories: category list; 
  final_jeopardy: final
}

(** [level_of_json json] is the representation of a level in [json]
    Requires:json is a valid JSON. *)
let level_of_json json = {
  score = json |> member "score" |> to_int;
  question = json |> member "question" |> to_string;
  answers = json |> member "answers" |> to_list |> List.map to_string;
  hint = json |> member "hint" |> to_string;
}

(** [category_of_json json] is the representation of a category in [json]
    Requires:json is a valid JSON. *)
let category_of_json json = {
  name = json |> member "name" |> to_string;
  levels = json |> member "levels" |> to_list |> List.map level_of_json;
}

(** [final_of_json json] is the representation of a final Jeopardy round in
    [json] Requires:json is a valid JSON. *)
let final_of_json json = 
  let i = Random.int (List.length json) in
  let q = List.nth json i in
  {
    final_question = q |> member "question" |> to_string;
    final_answers = q |> member "final answers" |> to_list |> 
                    List.map to_string;
  }

(* [from_json json] is a representation of a jeoparady game that json 
   represents. *)
let from_json json = {
  categories = json |> member "categories" |> to_list |> 
               List.map category_of_json;
  final_jeopardy = json |> member "finals" |> to_list |> final_of_json
}

(** [categories_list jeop] returns the names of the categories in [jeop]. *)
let categories_list jeop = 
  let rec get_categories (cats : category list) acc : category_name list =
    match cats with
    | [] -> acc
    | h::t -> get_categories t (h.name :: acc)
  in get_categories jeop.categories []

(** [from_categories json cats] is the same as [from_json] but only including
    the categories in [cats].
    raises [UnknownCategory] if an element in cats isn't already in json.
    raises [NoCategoriesProvided] if [cats] is empty.*)
let from_categories json (cats: category_name list) = 
  if List.length cats = 0 then raise NoCategoriesProvided else
    let rec helper acc = function
      | [] -> acc
      | h :: t -> if List.mem h.name cats 
        then helper (h :: acc) t 
        else helper acc t
    in
    let rec helper2 jeop_cats = function
      | [] -> ()
      | h :: t -> if List.mem h jeop_cats 
        then helper2 jeop_cats t 
        else raise (UnknownCategory h)
    in
    let jeop = from_json json in
    helper2 (categories_list jeop) cats;
    let new_cats = helper [] jeop.categories in
    {categories = new_cats; final_jeopardy = jeop.final_jeopardy}

(** [get_lowest_level jeop] is the amount of levels in the category in [jeop]
    with the least amount of levels *)
let get_lowest_level jeop =
  let rec helper acc = function
    | [] -> acc
    | h::t -> if List.length h.levels < acc
      then helper (List.length h.levels) t
      else helper acc t
  in
  helper 100 (jeop.categories)

let rec reduce_list n = function
  | [] -> []
  | h::t -> if n > 0 
    then h :: reduce_list (n-1) t
    else []

(** [reduce lowest_lev jeop] is jeop but with all the number of levels reduced
    to [lowest_lev]. *)
let reduce lowest_lev jeop : t = 
  let rec helper acc = function
    | [] -> acc
    | h::t -> if List.length (h.levels) > lowest_lev
      then 
        helper ({name=h.name; levels=(reduce_list lowest_lev h.levels)} :: acc) t
      else helper (h::acc) t
  in {categories=(helper [] jeop.categories); 
      final_jeopardy=jeop.final_jeopardy}

(* [category_name_string cat] is category_name [cat] as a string. *)
let category_name_string (cat: category_name) : string =
  cat

(* [category_name_string cat] is string [cat] as a category_name. *)
let category_name_from_string (cat: string) : category_name =
  cat

(** [categories jeop] returns the categories of the jeoparady game [jeop]. *)
let categories jeop = 
  jeop.categories

(** [get_category_name cat] returns the name of the category [cat] *)
let get_category_name (cat: category) = 
  cat.name

(** [get_level levels] returns the list of scores in the level list [levels]. *)
let rec get_levels (levels : level list) (acc : int list) : int list =
  match levels with
  | [] -> acc
  | h::t -> get_levels t (h.score :: acc)

(** [is_category cats cat] returns the category matching [cat] if [cat] is 
    a category in the category list [cats]. 
    Raises: UnknownCategory if [cat] is not a category. *)
let rec is_category (cats : category list) (cat : category_name) = 
  match cats with
  | [] -> raise (UnknownCategory cat)
  | h::t -> if (h.name = cat) then h else is_category t cat

(** [get_category_levels cat] returns the levels for category [cat]. *)
let get_category_levels (cat: category) = 
  cat.levels

(** [levels jeop cat] is the levels in category cat of jeoprady game [jeop]
    Raises: UnknownCategory if cat is an invalid category. *)
let levels jeop (cat : category_name) : int list =
  try (let categ = is_category jeop.categories cat in 
       (List.rev (get_levels (categ.levels) [])))
  with UnknownCategory cat -> raise (UnknownCategory cat)

(** [all_levels jeop] returns a list of all the levels in each category of 
    [jeop]*)
let all_levels jeop : int list list =
  let cats = categories jeop in
  let cat_names = List.map get_category_name cats in
  List.map (levels jeop) cat_names

(** [get_question levels score] returns the question associated with 
    level score [score] in the list of levels [levels]
    Raises: UnknownLevel if score is not a valid level. *)
let rec get_question (levels : level list) (score : int) : string =
  match levels with 
  | [] -> raise (UnknownLevel score)
  | h::t -> if h.score = score then h.question else get_question t score

(** [question jeop cat score] returns the question in the game [jeop]
    with category [cat] and level [score]
    Raises: UnknownLevel if score is not a valid level. *)
let question (jeop : t) (cat: category_name) (score : int) : string = 
  try (let categ = is_category jeop.categories cat in
       get_question categ.levels score)
  with UnknownLevel score -> raise (UnknownLevel score)

(** [answers jeop cat score] returns a list of answers of a question in category
    [cat] with score level [score]. Raises NoAnswersProvided if the category 
    is empty*)
let answers (jeop: t) (cat: category_name) (score: int) : string list =
  let rec helper = function
    | [] -> raise (NoAnswersProvided)
    | h::t -> if h.score = score then h.answers else helper t
  in      
  try (let categ = is_category jeop.categories cat in
       helper categ.levels)
  with UnknownCategory cat -> raise (UnknownCategory cat)

(** [score lev] is the score of level [lev]. *)
let score (lev : level) = 
  lev.score

(** [get_hint levels score] returns the hint associated with 
    level score [score] in the list of levels [levels]
    Raises: UnknownLevel if score is not a valid level. *)
let rec get_hint (levels : level list) (score : int) : string =
  match levels with 
  | [] -> raise (UnknownLevel score)
  | h::t -> if h.score = score then h.hint else get_hint t score

(** [hint jeop cat score] returns the hint in the game [jeop]
    with category [cat] and level [score]
    Raises: UnknownLevel if score is not a valid level.
    Raises: UnknownCategory if cat is an invalid category. *)
let hint (jeop : t) (cat: category_name) (score : int) : string = 
  try (let categ = is_category jeop.categories cat in
       get_hint categ.levels score)
  with 
  | UnknownCategory cat -> raise (UnknownCategory cat)
  | UnknownLevel score -> raise (UnknownLevel score) 

(** [final_jeopardy_question jeop] returns the question for the final round*)
let final_jeopardy_question (jeop : t) : string =
  jeop.final_jeopardy.final_question

(** [final_jeopardy_answers jeop] returns the answers for the final round*)
let final_jeopardy_answers (jeop : t) : string list =
  jeop.final_jeopardy.final_answers

(** [global_cats] is the list of all categories present in the master jeop.json
    file.  *)
let global_cats () = 
  let j = from_json (Yojson.Basic.from_file "jeop.json") in
  categories_list j