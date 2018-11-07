type phrase = string list

(** [command] represents the verbs that the players can input. It is either
    [Play], [Answer], [Score], [Quit], [Hint], or [Pass]. *)
type command = 
  | Play of phrase
  | Answer of phrase
  | Score
  | Quit
  | Hint
  | Pass
  | Skip
  | Double

exception Empty

exception Malformed 

(** [remove_empty lst acc] returns lst with all elements containing the empty
    string removed. *)
let rec remove_empty (lst : string list) acc : string list =
  match lst with
  | [] -> acc
  | h :: t -> if h = "" then remove_empty t acc
    else remove_empty t (acc @ [h])

(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes 
    the verb. The rest of the words, if any, become the object phrase.
    Examples: 
    - [parse "    play   Disney   100   "] is [Go ["Disney"; "100"]]
    - [parse "quit"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command
    is {i malformed} if the verb is neither "quit" nor "play" nor "score" nor 
    "who is" nor "what is", or if the verb is "quit" or "score" 
    and there is a non-empty object phrase,
    or if the verb is "play", "who is", or "what is" and there is an 
    empty object phrase.*)
let parse str : command =
  let split_list = remove_empty (String.split_on_char ' ' str ) [] in
  let lower_list = List.map String.lowercase_ascii split_list in
  match lower_list with
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
    else if h = "hint" then 
      if t <> [] then raise Malformed 
      else Hint
    else if h = "pass" then
      if t <> [] then raise Malformed
      else Pass
    else if h = "skip" then
      if t <> [] then raise Malformed
      else Skip
    else if h = "double" then
      if t <> [] then raise Malformed
      else Double
    else raise Malformed;
