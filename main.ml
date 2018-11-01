open Command

(** [question_loop jeop st lev cat] conducts the game loop after a question is
    asked, prompting the user, updating points in [st] once the user inputs a 
    valid answer, whether the answer is correct or not. 
    For one player games. *)
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
    | Score -> print_string "Very well, your score is: ";
      print_endline (string_of_int (State.current_score st));
      question_loop jeop st lev cat
    | Answer lst -> 
      (let answ = 
         String.trim (List.fold_left (fun x acc -> 
             x ^ " " ^ acc) "" lst) in                              
       let result = State.answer cat lev 
           (String.lowercase_ascii answ) jeop st in 
       match result with
       | Illegal -> 
         print_endline "That answer is illegal.";
         question_loop jeop st lev cat
       | Legal s -> 
         if (State.current_score s) > (State.current_score st) then
           ( print_endline "Congratulations, you are correct!";
             Unix.sleep 2;
             ANSITerminal.erase Screen;
             s)
         else (print_endline 
                 "Sorry that's wrong, better luck next time, buckaroo.";
               print_endline "The answer was: ";
               let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
               print_endline correct;
               Unix.sleepf 2.5;
               ANSITerminal.erase Screen;
               s))
    | Quit -> print_endline "OK, see ya next time!"; exit 0
    | Hint -> (print_endline "Here is a hint to help you:";
               let hint = Jeopardy.hint jeop cat lev in
               print_endline hint;
               match (State.hint cat lev jeop st) with
               | Legal st' -> question_loop jeop st' lev cat
               | Illegal -> 
                 print_endline 
                   "That is an invalid cat/lev combination (never happens)";
                 question_loop jeop st lev cat)
    | Pass -> print_endline "We will let you go this time!"; st

(** [play_loop jeop st] conducts the main game loop, prompting the user, 
    updating [st] until the user quits.
    For one player games. *)
let rec play_loop jeop (st : State.t) =
  if (List.length (State.current_categories st)) = 0 then 
    (print_endline ("You've finished the game!! Good job!");
     print_endline ("Your score was:");
     print_string ((string_of_int (State.current_score st)) ^ "\n");
     exit 0)
  else
    print_endline ("Here are the current categories and levels left:\n");
  print_string (State.current_board st);
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
               else(
                 print_endline "This is not an available level. Try again. \n";
                 play_loop jeop st))
          with | Jeopardy.UnknownCategory cat -> 
            print_endline 
              "That category doesn't exist/has already been played.";
            play_loop jeop st
               | Failure (int_of_string) -> 
                 print_endline 
                   "That's an invalid input (level needs to be a number)";
                 play_loop jeop st )
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
    | Hint -> print_endline "You need to choose a category and level first!";
      play_loop jeop st
    | Pass -> print_endline "You haven't even chosen a question yet!";
      play_loop jeop st

(** [question_loop_two_player jeop st lev cat] is the same as question_loop
    but for two players instead of one. *)
let rec question_loop_two_player jeop (st : State2players.t) 
    (lev: int) (cat: Jeopardy.category_name) : State2players.t = 
  match parse (read_line ()) with
  | exception Empty -> print_string "Enter an answer!"; 
    question_loop_two_player jeop st lev cat
  | exception Malformed -> 
    print_string 
      "That's an invalid response, make sure it's in the form of a question!\n";
    question_loop_two_player jeop st lev cat
  | comm -> match comm with
    | Play lst -> 
      print_endline "You picked a category, you need to answer the question.\n";
      question_loop_two_player jeop st lev cat
    | Score -> print_string "Very well, your score is: ";
      if State2players.get_current_player st = One then
        print_endline (string_of_int (State2players.current_player1_score st))
      else 
        print_endline (string_of_int (State2players.current_player2_score st));
      question_loop_two_player jeop st lev cat
    | Answer lst -> 
      (let answ = 
         String.trim (List.fold_left (fun x acc -> 
             x ^ " " ^ acc) "" lst) in                              
       let result = State2players.answer cat lev 
           (String.lowercase_ascii answ) jeop st in 
       match result with
       | Illegal -> 
         print_endline "That answer is illegal.";
         question_loop_two_player jeop st lev cat
       | Legal s -> 
         if State2players.get_current_player st = One then
           (if (State2players.current_player1_score s) > 
               (State2players.current_player1_score st) then
              (print_endline "Congratulations, you are correct!";
               Unix.sleep 2;
               ANSITerminal.erase Screen;
               s)
            else (print_endline 
                    "Sorry that's wrong, better luck next time, buckaroo.";
                  print_endline "The answer was: ";
                  let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
                  print_endline correct;
                  Unix.sleepf 2.5;
                  ANSITerminal.erase Screen;
                  s))
         else  
           (if (State2players.current_player2_score s) > 
               (State2players.current_player2_score st) then
              (print_endline "Congratulations, you are correct!";
               Unix.sleep 2;
               ANSITerminal.erase Screen;
               s)
            else (print_endline 
                    "Sorry that's wrong, better luck next time, buckaroo.";
                  print_endline "The answer was: ";
                  let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
                  print_endline correct;
                  Unix.sleepf 2.5;
                  ANSITerminal.erase Screen;
                  s)))
    | Quit -> print_endline "OK, see ya next time!"; exit 0
    | Hint -> (print_endline "Here is a hint to help you:";
               let hint = Jeopardy.hint jeop cat lev in
               print_endline hint;
               match (State2players.hint cat lev jeop st) with
               | Legal st' -> question_loop_two_player jeop st' lev cat
               | Illegal -> 
                 print_endline 
                   "That is an invalid cat/lev combination (never happens)";
                 question_loop_two_player jeop st lev cat)
    | Pass -> print_endline "We will let you go this time!"; st

(** [play_loop_two_player jeop st] is the same as [play_loop] but for two-player
    games. *)
let rec play_loop_two_player jeop (st : State2players.t) =
  if (List.length (State2players.current_categories st)) = 0 then 
    (let score1 = State2players.current_player1_score st in
     let score2 = State2players.current_player2_score st in
     print_endline ("You've finished the game!! Good job!");
     print_string ("The scores were: \nplayer 1: ");
     print_endline ((string_of_int score1));
     print_string ("player 2: ");
     print_endline ((string_of_int score2));
     if score1 > score2 then print_endline ("Congratulations player 1!")
     else if score2 > score1 then print_endline ("Congratulations player 2!")
     else print_endline ("You tied! Good Game.");
     exit 0)
  else
    print_endline ("Here are the current categories and levels left:\n");
  print_string (State2players.current_board st);
  if (State2players.get_current_player st = One) then print_string ("Player 1,")
  else print_string ("Player 2,");
  print_string ("please choose a category: ");
  match parse (read_line ()) with
  | exception Empty -> print_string "\nPlease choose an category!\n";
    play_loop_two_player jeop st
  | exception Malformed -> 
    print_string "\nThat's an invalid category score combination!\n";
    play_loop_two_player jeop st
  | comm -> match comm with
    | Play lst -> (* Check if question exists? *)
      (match lst with
       | cat::lev::[] -> 
         (try (let levels = State2players.current_category_levels st 
                   (Jeopardy.category_name_from_string cat) in
               if List.mem (int_of_string lev) levels then
                 (print_endline "OK, your question is ... \n";
                  print_endline (Jeopardy.question jeop 
                                   (Jeopardy.category_name_from_string cat) 
                                   (int_of_string lev));
                  let ques_result = 
                    question_loop_two_player jeop st (int_of_string lev) 
                      (Jeopardy.category_name_from_string cat) in                                                                               
                  let result = State2players.play 
                      (Jeopardy.category_name_from_string cat) 
                      (int_of_string lev) jeop ques_result in          
                  (match result with
                   | Legal t -> play_loop_two_player jeop t
                   | Illegal -> 
                     print_endline 
                       "That's not a valid category level combination.";
                     play_loop_two_player jeop st))
               else
                 (print_endline "This is not an available level. Try again. \n";
                  play_loop_two_player jeop st))
          with | Jeopardy.UnknownCategory cat -> 
            print_endline 
              "That category doesn't exist/has already been played.";
            play_loop_two_player jeop st
               | Failure (int_of_string) -> 
                 print_endline 
                   "That's an invalid input (level needs to be a number)";
                 play_loop_two_player jeop st )
       | _ -> print_endline "You forgot to type a category or score";
         play_loop_two_player jeop st)
    | Answer lst -> 
      print_endline 
        "You need to choose a category right now, not answer a question.";
      play_loop_two_player jeop st
    | Score -> print_endline "Very well, your score is: ";
      if State2players.get_current_player st = One then 
        print_endline (string_of_int (State2players.current_player1_score st))
      else 
        print_endline (string_of_int (State2players.current_player2_score st));
      play_loop_two_player jeop st  
    | Quit -> print_endline "OK, see ya next time!"; exit 0
    | Hint -> print_endline "You need to choose a category and level first!";
      play_loop_two_player jeop st
    | Pass -> print_endline "You haven't even chosen a question yet!";
      play_loop_two_player jeop st

(** [play_game f] starts the jeopardy in file [f]. *)
let rec play_game f =
  ANSITerminal.resize 165 40;
  ANSITerminal.(print_string [red] "
                                                                                                                            dddddddd                        
             jjjj                                                                                                           d::::::d                        
            j::::j                                                                                                          d::::::d                        
             jjjj                                                                                                           d::::::d                        
                                                                                                                            d:::::d                         
           jjjjjjj    eeeeeeeeeeee       ooooooooooo   ppppp   ppppppppp     aaaaaaaaaaaaa  rrrrr   rrrrrrrrr       ddddddddd:::::dyyyyyyy           yyyyyyy
           j:::::j  ee::::::::::::ee   oo:::::::::::oo p::::ppp:::::::::p    a::::::::::::a r::::rrr:::::::::r    dd::::::::::::::d y:::::y         y:::::y 
            j::::j e::::::eeeee:::::eeo:::::::::::::::op:::::::::::::::::p   aaaaaaaaa:::::ar:::::::::::::::::r  d::::::::::::::::d  y:::::y       y:::::y  
            j::::je::::::e     e:::::eo:::::ooooo:::::opp::::::ppppp::::::p           a::::arr::::::rrrrr::::::rd:::::::ddddd:::::d   y:::::y     y:::::y   
            j::::je:::::::eeeee::::::eo::::o     o::::o p:::::p     p:::::p    aaaaaaa:::::a r:::::r     r:::::rd::::::d    d:::::d    y:::::y   y:::::y    
            j::::je:::::::::::::::::e o::::o     o::::o p:::::p     p:::::p  aa::::::::::::a r:::::r     rrrrrrrd:::::d     d:::::d     y:::::y y:::::y     
            j::::je::::::eeeeeeeeeee  o::::o     o::::o p:::::p     p:::::p a::::aaaa::::::a r:::::r            d:::::d     d:::::d      y:::::y:::::y      
            j::::je:::::::e           o::::o     o::::o p:::::p    p::::::pa::::a    a:::::a r:::::r            d:::::d     d:::::d       y:::::::::y       
            j::::je::::::::e          o:::::ooooo:::::o p:::::ppppp:::::::pa::::a    a:::::a r:::::r            d::::::ddddd::::::dd       y:::::::y        
            j::::j e::::::::eeeeeeee  o:::::::::::::::o p::::::::::::::::p a:::::aaaa::::::a r:::::r             d:::::::::::::::::d        y:::::y         
            j::::j  ee:::::::::::::e   oo:::::::::::oo  p::::::::::::::pp   a::::::::::aa:::ar:::::r              d:::::::::ddd::::d       y:::::y          
            j::::j    eeeeeeeeeeeeee     ooooooooooo    p::::::pppppppp      aaaaaaaaaa  aaaarrrrrrr               ddddddddd   ddddd      y:::::y           
            j::::j                                      p:::::p                                                                          y:::::y            
  jjjj      j::::j                                      p:::::p                                                                         y:::::y             
 j::::jj   j:::::j                                     p:::::::p                                                                       y:::::y              
 j::::::jjj::::::j                                     p:::::::p                                                                      y:::::y               
  jj::::::::::::j                                      p:::::::p                                                                     yyyyyyy                
    jjj::::::jjj                                       ppppppppp                                                                                            
       jjjjjj                                                                                                                                               
  \n");
  let jeop = Jeopardy.from_json (Yojson.Basic.from_file f) in
  ANSITerminal.(print_string [red] 
                  "Here are the rules of Jeopardy:
   You're allowed to play with either one or two players and answer trivia 
   questions in order to get the highest amount of points. For each turn the 
   player can choose a question from the board by choosing the category and 
   point value. EG: play Music 300. You are then presented with a question, 
   but you MUST answer it in the form of a question ('who is' or 'what is', 
   etc). If you answer the question correctly your score goes up by however 
   much the question was worth, if you get it wrong, you lose that many points. 
   You can also 'pass' and recieve no points. You can also ask for a 'hint' for 
   every question, but this costs 100 points each time. At any point you check 
   your score with 'score' or quit the game with 'quit'. Have fun and good 
   luck!\n");
  ANSITerminal.(print_string [red] "\n 1 or 2 players?\n");
  match read_line() with
  | "One" | "1" | "one" -> 
    let st = State.init_state jeop in
    (play_loop jeop st)
  | "Two" | "2" | "two" -> 
    let st = State2players.init_state jeop in
    play_loop_two_player jeop st
  | _ -> print_endline "You can only have one or two players";
    play_game f

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  (* ANSITerminal.resize 140 40; *)
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our jeopardy game extravaganza!\n");
  print_endline "Enter the name of the jeopardy file you want to play.";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> ANSITerminal.erase Screen;
    play_game file_name

(* Execute the game engine. *)
let () = main ()