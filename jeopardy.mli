(** The type of questions. *)
type question

(** The type of category names. *)
type category_name

(** [level] represents a level in a category that has a score, a question, 
    a list of possible correct answers, and a hint for the answers. *)
type level

(** [category] represents a jeopardy category that has a name and a list of
    levels in that category. *)
type category

(** [t] represents a jeopardy game, which includes a list of categories, each
    with levels to choose from. *)
type t 

exception UnknownCategory of category_name

exception UnknownLevel of int

exception NoAnswersProvided

(* [from_json json] is a representation of a jeoparady game that json 
   represents. *)
val from_json : Yojson.Basic.json -> t

(* [category_name_string cat] is category_name [cat] as a string. *)
val category_name_string : category_name -> string

(* [category_name_string cat] is string [cat] as a category_name. *)
val category_name_from_string : string -> category_name

(** [categories jeop] returns the categories of the jeoparady game [jeop]. *)
val categories : t -> category list

(** [categories_list jeop] returns the names of the categories in [jeop]. *)
val categories_list : t -> category_name list

(** [get_category_name cat] returns the name of the cateogry [cat] *)
val get_category_name : category -> category_name

(** [get_level levels] returns the list of scores in the level list [levels]. *)
val get_levels : level list -> int list -> int list

(** [is_category cats cat] returns the category matching [cat] if [cat] is 
    a category in the category list [cats]. 
    Raises: UnknownCategory if [cat] is not a category. *)
val is_category : category list -> category_name -> category

(** [get_category_levels cat] returns the levels for category [cat]. *)
val get_category_levels : category -> level list

(** [levels jeop cat] is the levels in category cat of jeoprady game [jeop]
    Raises: UnknownCategory if cat is an invalid category. *)
val levels : t -> category_name -> int list

(** [all_levels jeop] returns a list of all the levels in each category of 
    [jeop]*)
val all_levels : t -> int list list

(** [question jeop cat score] returns the question in the game [jeop]
    with category [cat] and level [score]
    Raises: UnknownLevel if score is not a valid level. *)
val question : t -> category_name -> int -> string

(** [answers jeop cat score] returns a list of answers of the question in 
    category [cat] with score level [score]. [None] if the category is empty.*)
val answers : t -> category_name -> int -> string list

(** [score lev] is the score of level [lev]. *)
val score : level -> int

(** [hint jeop cat score] returns the hint in the game [jeop]
    with category [cat] and level [score]
    Raises: UnknownLevel if score is not a valid level. *)
val hint: t -> category_name -> int -> string