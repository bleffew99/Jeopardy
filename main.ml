open Command

let rec play_loop jeop (st : State.t) =
  if (List.length (State.current_categories st)) = 0 then 
    print_endline ("You've beat the game!! Good job!")
  else 
    print_endline ("Here are the current categories and levels left:\n");
  print_string (State.current_category_levels_to_string st);


  let play_game f =
    let jeop = Jeopardy.from_json (Yojson.Basic.from_file f) in
    let st = State.init_state jeop in
    (play_loop jeop st)

(** [main]  *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our jeopardy game extravaganza!\n");
  print_endline "Enter the name of the jeopardy file you want to play.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

let () = main ()