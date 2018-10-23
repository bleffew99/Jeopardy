type phrase = string list

type command = 
  | Play of phrase
  | Answer of phrase
  | Score 
  | Quit

exception Empty

exception Malformed

val remove_empty : string list -> string list -> string list 

val parse : string -> command

