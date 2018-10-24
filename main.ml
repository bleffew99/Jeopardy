open Command

(** [question_loop jeop st lev cat] conducts the game loop after a question is
    asked, prompting the user, updating points in [st] once the user inputs a 
    valid answer, whether the answer is correct or not. *)
let rec question_loop jeop (st : State.t) 
    (lev: int) (cat: Jeopardy.category_name) : State.t = 
  match parse (read_line ()) with
  | exception Empty -> print_string "Enter an answer!"; 
    question_loop jeop st lev cat
  | exception Malformed -> 
    print_string 
      "That's an invalid response, make sure it's in the form of a question!\n";
    question_loop jeop st lev cat
  | comm -> match comm with
    | Play lst -> 
      print_endline "You picked a category, you need to answer the question.\n";
      question_loop jeop st lev cat
    | Score -> print_endline "Very well, your score is: ";
      print_string ((string_of_int (State.current_score st)) ^ "\n");
      question_loop jeop st lev cat
    | Answer lst -> 
      (let answ = 
         String.trim (List.fold_left (fun x acc -> x ^ " " ^ acc) "" lst) in
       let result = State.answer cat lev answ jeop st in 
       match result with
       | Illegal -> 
         print_endline "That answer is illegal.";
         question_loop jeop st lev cat
       | Legal s -> 
         if (State.current_score s) > (State.current_score st) then
           (print_endline "Congratulations, you are correct!"; 
            s)
         else (print_endline 
                 "Sorry that's wrong, better luck next time, buckaroo."; s))
    | Quit -> print_endline "OK, see ya next time!"; exit 0

(** [play_loop jeop st] conducts the main game loop, prompting the user, 
    updating [st] until the user quits *)
let rec play_loop jeop (st : State.t) =
  if (List.length (State.current_categories st)) = 0 then 
    (print_endline ("You've finished the game!! Good job!");
     print_endline ("Your score was:");
     print_string ((string_of_int (State.current_score st)) ^ "\n");
     exit 0)
  else 
    print_endline ("Here are the current categories and levels left:\n");
  print_string (State.current_category_levels_to_string st);
  print_string ("Please choose a category: ");
  match parse (read_line ()) with
  | exception Empty -> print_string "\nPlease choose an category!\n";
    play_loop jeop st
  | exception Malformed -> 
    print_string "\nThat's an invalid category score combination!\n";
    play_loop jeop st
  | comm -> match comm with
    | Play lst -> (* Check if question exists? *)
      (match lst with
       | cat::lev::[] -> 
         (try (let levels = State.current_category_levels st 
                   (Jeopardy.category_name_from_string cat) in
               if List.mem (int_of_string lev) levels then
                 (print_endline "OK, your question is ... \n";
                  print_endline (Jeopardy.question jeop 
                                   (Jeopardy.category_name_from_string cat) 
                                   (int_of_string lev));
                  let ques_result = question_loop jeop st (int_of_string lev) 
                      (Jeopardy.category_name_from_string cat) in                                                                               
                  let result = State.play 
                      (Jeopardy.category_name_from_string cat) 
                      (int_of_string lev) jeop ques_result in          
                  (match result with
                   | Legal t -> play_loop jeop t
                   | Illegal -> 
                     print_endline 
                       "That's not a valid category level combination.";
                     play_loop jeop st))
               else
                 print_endline "This is not an available level. Try again. \n")
          with | Jeopardy.UnknownCategory cat -> 
            print_endline 
              "That category doesn't exist/has already been played.";
            play_loop jeop st)
       | _ -> print_endline "You forgot to type a category or score";
         play_loop jeop st)
    | Answer lst -> 
      print_endline 
        "You need to choose a category right now, not answer a question.";
      play_loop jeop st
    | Score -> print_endline "Very well, your score is: ";
      print_endline (string_of_int (State.current_score st));
      play_loop jeop st  
    | Quit -> print_endline "OK, see ya next time!"; exit 0

(** [play_game f] starts the jeopardy in file [f]. *)
let play_game f =
  let jeop = Jeopardy.from_json (Yojson.Basic.from_file f) in
  let st = State.init_state jeop in
  (play_loop jeop st)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our jeopardy game extravaganza!\n");
  print_endline "Enter the name of the jeopardy file you want to play.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()