type phrase = string list

type command = 
  | Play of phrase
  | Answer of phrase
  | Score
  | Quit

exception Empty

exception Malformed 

(** [remove_empty lst acc] returns lst with all elements containing the empty
    string removed. *)
let rec remove_empty (lst : string list) acc : string list =
  match lst with
  | [] -> acc
  | h :: t -> if h = "" then remove_empty t acc
    else remove_empty t (acc @ [h])

let parse str : command =
  let split_list = remove_empty (String.split_on_char ' ' str ) [] in
  match split_list with
  | [] -> raise Empty
  | h::t -> 
    if h = "play" then 
      if (List.length t) <> 2 then raise Malformed
      else (Play t)
    else if h = "score" then
      if t <> [] then raise Malformed 
      else Score
    else if h = "quit" then
      if t <> [] then raise Malformed
      else (Quit)
    else if  (h = "what" || h = "who") then (
      match t with
      | h1::t1 -> if (h1 = "is" || h1 = "are" || h1 = "was" || h1 = "were")
        then if t1 = [] then raise Malformed
          else (Answer t1) else raise Malformed
      | [] -> raise Malformed)
    else raise Malformed