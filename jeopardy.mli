type question

type category_name

type level

type category

type t 

exception UnknownCategory of category_name

exception UnknownLevel of int

exception NoAnswersProvided

val from_json : Yojson.Basic.json -> t

val category_name_string : category_name -> string

val categories : t -> category list

val categories_list : t -> category_name list

val get_category_name : category -> category_name

val get_levels : level list -> int list -> int list

val is_category : category list -> category_name -> category

val get_category_levels : category -> level list

val levels : t -> category_name -> int list

val question : t -> category_name -> int -> string

val answers : t -> category_name -> int -> string list

val score : level -> int