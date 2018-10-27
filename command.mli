type phrase = string list

type command = 
  | Play of phrase
  | Answer of phrase
  | Score 
  | Quit
  | Hint
  | Pass

exception Empty

exception Malformed

(** [remove_empty lst acc] returns lst with all elements containing the empty
    string removed. *)
val remove_empty : string list -> string list -> string list 

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
val parse : string -> command

