type phrase = string list

type command = 
  | Play of phrase
  | Answer of phrase
  | Score 

exception Empty

exception Malformed 

(** [remove_empty lst acc] returns lst with all elements containing the empty
    string removed. *)
let rec remove_empty (lst : string list) acc : string list =
  match lst with
  | [] -> acc
  |h :: t -> if h = "" then remove_empty t acc
    else remove_empty t (acc @ [h])

let parse str : command =
  let split_list = remove_empty (String.split_on_char ' ' str ) [] in
  match split_list with
  | [] -> raise Empty
  | h1::h2::t ->
    if (h1 = "what" || h1 = "who") && 
       (h2 = "is" || h2 = "are" || h2 = "was" || h2 = "were")
    then if t = [] then raise Malformed
      else (Answer t)
    else raise Malformed
  | h::t -> 
    if h = "play" then 
      if (List.length t) <> 2 then raise Malformed
      else (Play t)
    else if h = "score" then
      if t <> [] then raise Malformed 
      else Score
    else raise Malformed