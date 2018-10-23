open Yojson.Basic.Util

type question = string

type category_name = string

exception UnknownCategory of category_name
exception UnknownLevel of int
exception NoAnswersProvided

type level = {
  score: int;
  question: question;
  answers: string list;
}

type category = {
  name: category_name;
  levels: level list;
}

type t = {
  categories: category list;  
}
(** [level_of_json json] is the representation of a level in [json]
    Requires:json is a valid JSON*)
let level_of_json json = {
  score = json |> member "score" |> to_int;
  question = json |> member "question" |> to_string;
  answers = json |> member "answers" |> to_list |> List.map to_string;
}
(** [category_of_json json] is the representation of a category in [json]
    Requires:json is a valid JSON*)
let category_of_json json = {
  name = json |> member "name" |> to_string;
  levels = json |> member "levels" |> to_list |> List.map level_of_json;
}
(* [from_json json] is a representation of a jeoparady game that json represents)
*)
let from_json json = {
  categories = json |> member "categories" |> to_list |> 
               List.map category_of_json;
}

(* [category_name_string cat] is category_name [cat] as a string. *)
let category_name_string (cat: category_name) : string =
  cat

(* [category_name_string cat] is string [cat] as a category_name. *)
let category_name_from_string (cat: string) : category_name =
  cat

(** [categories jeop] returns the categories of the jeoparady game [jeop] *)
let categories jeop = 
  jeop.categories

let categories_list jeop = 
  let rec get_categories (cats : category list) acc : category_name list =
    match cats with
    | [] -> acc
    | h::t -> get_categories t (h.name :: acc)
  in get_categories jeop.categories []

(** [get_category_name cat] returns the name of the cateogry [cat] *)
let get_category_name (cat: category) = 
  cat.name

(** [get_level levels] returns the list of scores in the level [levels]*)
let rec get_levels (levels : level list) (acc : int list) : int list=
  match levels with
  | [] -> acc
  | h::t -> get_levels t (h.score :: acc)

(** [is_category cats cat] returns the category matching [cat] if [cat] is 
    a category in the category list [cats]
    Raises UnknownCategory if [cat] is not a category*)
let rec is_category (cats : category list) (cat : category_name) = 
  match cats with
  | [] -> raise (UnknownCategory cat)
  | h::t -> if (h.name = cat) then h else is_category t cat

let get_category_levels (cat: category) = 
  cat.levels

(** [levels jeop cat] is the levels in category cat of jeoprady game [jeop]
    Raises UnknownCategory if cat is an invalid category *)
let levels jeop (cat : category_name) : int list =
  try (let categ = is_category jeop.categories cat in 
       (List.rev (get_levels (categ.levels) [])))
  with UnknownCategory cat -> raise (UnknownCategory cat)

(** [get_question levels score] returns the question associated with 
     level score [score] in the list of levels [levels]
     Raises UnknownLevel if score is not a valid level*)
let rec get_question (levels : level list) (score : int) : string =
  match levels with 
  | [] -> raise (UnknownLevel score)
  | h::t -> if h.score = score then h.question else get_question t score

(** [question jeop cat score] returns the question in the game [jeop]
    with category [cat] and level [score]
    Raies UnknownLevel if score is not a valid level*)
let question (jeop : t) (cat: category_name) (score : int) : string = 
  try (let categ = is_category jeop.categories cat in
       get_question categ.levels score)
  with UnknownLevel score -> raise (UnknownLevel score)

(** [answers jeop cat score] returns a list of answers of the question in category
    [cat] with score level [score]. None if the category is empty*)
let answers (jeop: t) (cat: category_name) (score: int) : string list =
  let rec helper = function
    | [] -> raise (NoAnswersProvided)
    | h::t -> if h.score = score then h.answers else helper t
  in      
  try (let categ = is_category jeop.categories cat in
       helper categ.levels)
  with UnknownCategory cat -> raise (UnknownCategory cat)

let question_of_level (lev: level) =
  lev.question

let score (lev : level) = 
  lev.score

