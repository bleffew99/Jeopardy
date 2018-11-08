open Command

(** [double_loop jeop st lev cat] updates the player's score in [st] to a new 
    score after the player tries to use the double-or-nothing ability,
    getting twice the points of level [lev] for getting it right and losing
    twice the points for getting it wrong. The  player's status for
    used double is also set to [true], and the player is only allowed to use
    the ability once. *)
let rec double_loop jeop (st : State.t) (lev: int) 
    (cat: Jeopardy.category_name) =
  print_endline 
    ("You will get either double the points if you get this question"
     ^ " right, or lose double the points if you get it wrong! What is your" ^
     " answer?");
  let answer = String.lowercase_ascii (read_line()) in
  let answer_lst = remove_empty (String.split_on_char ' ' answer ) [] in
  match answer_lst with
  | h::m::t -> 
    if (h = "what" || h = "who" || h = "when") &&
       (m = "is" || m = "are" || m = "was" || m = "were")
    then 
      let ans = String.trim (List.fold_left (fun x acc -> 
          x ^ " " ^ acc) "" t) in   
      (match State.double st jeop cat lev ans with
       | Legal s -> 
         ((if State.current_score s > State.current_score st then
             (print_endline "You are correct! You got double the points!!")
           else 
             (print_endline 
                ("Yikes, you got that wrong! You lose twice the points. "
                 ^ "The answer was: ");
              let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
              print_endline correct));
          Unix.sleepf 2.5;
          ANSITerminal.erase Screen;
          s)
       | Illegal -> print_endline "You've already used your Double chance!"; 
         print_endline "Enter an answer!";
         st
       | exception Jeopardy.NoAnswersProvided -> 
         print_endline "Write an answer!"; 
         double_loop jeop st lev cat
       | exception Jeopardy.UnknownCategory cat -> 
         print_endline "This is wrong!"; 
         double_loop jeop st lev cat)
    else 
      (print_endline "Answer in a correct question format!";
       double_loop jeop st lev cat)
  | _ -> print_endline "Answer in the question format!";
    double_loop jeop st lev cat

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
    | Pass -> (match (State.pass st) with
        | Illegal -> 
          print_endline "You have used up all your passes! You have to answer.";
          question_loop jeop st lev cat
        | Legal s -> 
          print_string "We will let you go this time! Your passes left:"; 
          print_endline (string_of_int (State.current_passes_left s));
          s)
    | Skip -> 
      print_endline "This power does not exist here.";
      question_loop jeop st lev cat
    | Double -> 
      if State.has_used_double st then 
        (print_endline "You have already used up your double chance!";
         print_endline "Answer the question please.";
         question_loop jeop st lev cat)
      else double_loop jeop st lev cat

(** [final_answer_loop jeop st] conducts a loop for the final jeopardy round,
    prompting the user for an answer and updating the final score accordingly.*)
let rec final_answer_loop jeop (st: State.t) : State.t =
  print_endline ("Here is the FINAL JEOPDARDY question:");
  print_endline (Jeopardy.final_jeopardy_question jeop);
  let answer = String.lowercase_ascii (read_line()) in
  let answer_lst = remove_empty (String.split_on_char ' ' answer ) [] in
  match answer_lst with
  | h::m::t -> 
    if (h = "what" || h = "who" || h = "when") &&
       (m = "is" || m = "are" || m = "was" || m = "were")
    then 
      let ans = String.trim (List.fold_left (fun x acc -> 
          x ^ " " ^ acc) "" t) in   
      match State.final_answer jeop st ans with
      | Legal s -> s
      | Illegal -> print_endline "This is wrong"; st
    else 
      (print_endline "Answer in a correct question format!";
       final_answer_loop jeop st)
  | _ -> print_endline "Answer in the question format!";
    final_answer_loop jeop st

(** [final_bet_loop jeop st] conducts a loop for the player to bet any amount of
    the player's points for the final jeopardy round, prompting the user for 
    a bet and updating the bet in [st], then calling the [final_answer_loop] on
    the new state for the player to answer the question.*)
let rec final_bet_loop jeop (st : State.t) : State.t =
  print_endline ("Welcome to FINAL JEOPARDY!");
  print_string ("Your current score is: ");
  print_endline (string_of_int (State.current_score st));
  print_endline ("You could win double the points you bet. Place your bet:");
  try (let bet = int_of_string (read_line()) in
       if bet < 0 then
         (print_endline 
            "Do you want to lose points?!!!!! Bet a positive number!";
          final_bet_loop jeop st)
       else if bet > State.current_score st then 
         (print_endline "You don't have that many points! Don't be greedy.";
          final_bet_loop jeop st)
       else match State.bet st bet with
         | Legal s -> final_answer_loop jeop s
         | Illegal -> print_endline ("This is wrong"); st)
  with 
  | Failure (int_of_string) -> 
    print_endline 
      "That's an invalid input (bet needs to be a number)";
    final_bet_loop jeop st 

(** [play_loop jeop st] conducts the main game loop, prompting the user, 
    updating [st] until the user quits or finishes the game.
    For one player games. *)
let rec play_loop jeop (st : State.t) =
  if (List.length (State.current_categories st)) = 0 then
    (let score = State.current_score st in
     if not (State.has_played_final st) && score < 0 then
       (print_endline ("You've finished the game!! Good job!");
        print_endline ("Since you do not have a positive score, there is "
                       ^ "no need for a final jeopardy round!");
        print_string ("Your score was:");
        print_endline (string_of_int score);
        exit 0
       )
     else if State.has_played_final st then
       (print_endline ("You've finished the game!! Good job!");
        print_string ("Your score was:");
        print_endline ((string_of_int (State.current_score st)) ^ "\n");
        exit 0)
     else
       play_loop jeop (final_bet_loop jeop st))
  else
    (print_endline ("Here are the current categories and levels left:\n");
     let board = State.current_board st in
     ANSITerminal.(print_string [blue] (board ^ "\n"));
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
       | Skip -> print_endline "You haven't even chosen a question yet!";
         play_loop jeop st
       | Double -> print_endline "You haven't even chosen a question yet!";
         play_loop jeop st)

(** [double_two_loop jeop st lev cat] updates the current player's score in [st]
    to a new score after the player tries to use the double-or-nothing ability,
    getting twice the points of level [lev] for getting it right and losing
    twice the points for getting it wrong. The current player's status for
    used double is also set to [true], and each player is only allowed to use
    the ability once. *)
let rec double_two_loop jeop (st : State2players.t) (lev: int) 
    (cat: Jeopardy.category_name) =
  print_endline 
    ("You will get either double the points if you get this question"
     ^ " right, or lose double the points if you get it wrong! What is your" ^
     " answer?");
  let answer = String.lowercase_ascii (read_line()) in
  let answer_lst = remove_empty (String.split_on_char ' ' answer ) [] in
  match answer_lst with
  | h::m::t -> 
    if (h = "what" || h = "who" || h = "when") &&
       (m = "is" || m = "are" || m = "was" || m = "were")
    then 
      let ans = String.trim (List.fold_left (fun x acc -> 
          x ^ " " ^ acc) "" t) in   
      (match State2players.double st jeop cat lev ans with
       | Legal s -> 
         (if State2players.get_current_player st = One then
            (if State2players.current_player1_score s > 
                State2players.current_player1_score st then
               (print_endline "You are correct! You got double the points!!")
             else 
               (print_endline 
                  ("Yikes, you got that wrong! You lose twice the points. "
                   ^ "The answer was: ");
                let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
                print_endline correct);
             Unix.sleepf 2.5;
             ANSITerminal.erase Screen;
             s)
          else
            (if State2players.current_player2_score s > 
                State2players.current_player2_score st then
               print_endline "You are correct! You got double the points!!"
             else 
               (print_endline 
                  ("Yikes, you got that wrong! You lose twice the points. "
                   ^ "The answer was: ");
                let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
                print_endline correct);
             Unix.sleepf 2.5;
             ANSITerminal.erase Screen;
             s))
       | Illegal -> print_endline "You've already used your Double chance!"; 
         print_endline "Enter an answer!";
         st
       | exception Jeopardy.NoAnswersProvided -> 
         print_endline "Write an answer!"; 
         double_two_loop jeop st lev cat
       | exception Jeopardy.UnknownCategory cat -> 
         print_endline "This is wrong!"; 
         double_two_loop jeop st lev cat)
    else 
      (print_endline "Answer in a correct question format!";
       double_two_loop jeop st lev cat)
  | _ -> print_endline "Answer in the question format!";
    double_two_loop jeop st lev cat

(** [skip_two_loop jeop st lev cat] lets the current player skip the other 
    player's next turn and play twice in a row, updating the player's score
    after the player answers the question. Each player can only use the skip
    ability once in the game. *)
let rec skip_two_loop jeop (st : State2players.t) (lev: int) 
    (cat: Jeopardy.category_name) =
  print_endline "Answer the question";
  let answer = String.lowercase_ascii (read_line()) in
  let answer_lst = remove_empty (String.split_on_char ' ' answer ) [] in
  match answer_lst with
  | h::m::t -> ( 
      if (h = "what" || h = "who" || h = "when") &&
         (m = "is" || m = "are" || m = "was" || m = "were")
      then ( 
        let ans = String.trim (List.fold_left (fun x acc -> 
            x ^ " " ^ acc) "" t) in   
        match State2players.skip cat lev ans jeop st with 
        | Legal s -> (
            if State2players.get_current_player st = One then
              if State2players.current_player1_score s > 
                 State2players.current_player1_score st 
              then
                (print_endline "You are correct! You go again!!"; s)
              else 
                (print_endline 
                   ("Yikes, you got that wrong! It's still your turn though. "
                    ^ "The answer was: ");
                 let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
                 print_endline correct;
                 Unix.sleepf 2.5;
                 ANSITerminal.erase Screen;
                 s)
            else 
              (if State2players.current_player2_score s > 
                  State2players.current_player2_score st 
               then (print_endline "You are correct! You go again!!!";
                     s)
               else 
                 (print_endline 
                    ("Yikes, you got that wrong! It's still your turn though.  "
                     ^ "The answer was: ");
                  let correct = List.nth (Jeopardy.answers jeop cat lev) 0 in
                  print_endline correct;
                  Unix.sleepf 2.5;
                  ANSITerminal.erase Screen;
                  s)))
        | Illegal -> (print_endline "You've already used your skip chance!"; 
                      st)
        | exception Jeopardy.NoAnswersProvided -> 
          print_endline "Write an answer!"; 
          skip_two_loop jeop st lev cat
        | exception Jeopardy.UnknownCategory cat -> 
          print_endline "This is wrong!"; 
          skip_two_loop jeop st lev cat)
      else 
        (print_endline "Answer in a correct question format!";
         skip_two_loop jeop st lev cat))
  | _ -> (print_endline "Answer in the question format!";
          skip_two_loop jeop st lev cat)

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
    | Pass -> (match (State2players.pass st) with
        | Illegal -> 
          print_endline "You have used up all your passes! You have to answer.";
          question_loop_two_player jeop st lev cat
        | Legal s -> 
          print_string "We will let you go this time! Your passes left: "; 
          if State2players.get_current_player st = One then
            (print_endline 
               (string_of_int (State2players.player1_passes_left s));
             s)
          else 
            (print_endline 
               (string_of_int (State2players.player2_passes_left s));
             s)
      )
    | Skip -> (
        if State2players.current_player_used_skip st then
          (print_endline "You already used up your skip chance!";
           print_endline "Answer the question please.";
           question_loop_two_player jeop st lev cat)
        else
          (print_endline "Ok, the next player's turn will be skipped";
           skip_two_loop jeop st lev cat))
    | Double -> 
      if State2players.get_current_player st = One then
        (if State2players.player1_double_used st then 
           (print_endline "You have already used up your double chance!";
            print_endline "Answer the question please.";
            question_loop_two_player jeop st lev cat)
         else double_two_loop jeop st lev cat)
      else 
        (if State2players.player2_double_used st then 
           (print_endline "You have already used up your double chance!";
            print_endline "Answer the question please.";
            question_loop_two_player jeop st lev cat)
         else double_two_loop jeop st lev cat)

(** [final_answer1_loop jeop st] prompts player1 for an answer to the final
    jeopardy question until player1 inputs a valid answer, and returns that 
    string answer.*)
let rec final_answer1_loop jeop (st: State2players.t) : string =
  print_endline ("Here is the FINAL JEOPDARDY question:");
  print_endline (Jeopardy.final_jeopardy_question jeop);
  ANSITerminal.(print_string [red] "Player 1, what is your answer?\n");
  let answer1 = String.lowercase_ascii (read_line()) in
  let answer_lst1 = remove_empty (String.split_on_char ' ' answer1 ) [] in
  match answer_lst1 with
  | h::m::t -> 
    if (h = "what" || h = "who" || h = "when") &&
       (m = "is" || m = "are" || m = "was" || m = "were")
    then 
      (ANSITerminal.erase Screen;
       let ans = String.trim (List.fold_left (fun x acc -> 
           x ^ " " ^ acc) "" t) in   
       ans)
    else 
      (print_endline "Answer in a correct question format!";
       final_answer1_loop jeop st)
  | _ -> print_endline "Answer in the question format!";
    final_answer1_loop jeop st

(** [final_answer2_loop jeop st] prompts player2 for an answer to the final
    jeopardy question until player2 inputs a valid answer, and returns that 
    string answer.*)
let rec final_answer2_loop jeop (st: State2players.t) : string =
  print_endline ("Here is the FINAL JEOPDARDY question:");
  print_endline (Jeopardy.final_jeopardy_question jeop);
  ANSITerminal.(print_string [red] "Player 2, what is your answer?\n");
  let answer2 = String.lowercase_ascii (read_line()) in
  let answer_lst2 = remove_empty (String.split_on_char ' ' answer2) [] in
  match answer_lst2 with
  | h::m::t -> 
    if (h = "what" || h = "who" || h = "when") &&
       (m = "is" || m = "are" || m = "was" || m = "were")
    then 
      (ANSITerminal.erase Screen;
       let ans = String.trim (List.fold_left (fun x acc -> 
           x ^ " " ^ acc) "" t) in   
       ans)
    else 
      (print_endline "Answer in a correct question format!";
       final_answer2_loop jeop st)
  | _ -> print_endline "Answer in the question format!";
    final_answer2_loop jeop st

(** [final_bet1_loop jeop st] prompts player1 for a bet for the final jeopardy
    round until player1 inputs a valid integer bet, and returns that bet. *)
let rec final_bet1_loop jeop (st : State2players.t) : int =
  print_endline ("Welcome to FINAL JEOPARDY!");
  print_string ("Player1, your current score is: ");
  print_endline (string_of_int (State2players.current_player1_score st));
  print_string ("Player2's score is: ");
  print_endline (string_of_int (State2players.current_player2_score st));
  print_endline ("You could win double the points you bet. Place your bet:");
  try (let bet = int_of_string (read_line()) in
       if bet < 0 then
         (print_endline 
            "Do you want to lose points?!!!!! Bet a positive number!";
          final_bet1_loop jeop st)
       else if bet > State2players.current_player1_score st then 
         (print_endline "You don't have that many points! Don't be greedy.";
          final_bet1_loop jeop st)
       else 
         (ANSITerminal.erase Screen;
          bet))
  with 
  | Failure (int_of_string) -> 
    print_endline 
      "That's an invalid input (bet needs to be a number)";
    final_bet1_loop jeop st 

(** [final_bet2_loop jeop st] prompts player2 for a bet for the final jeopardy
    round until player2 inputs a valid integer bet, and returns that bet. *)
let rec final_bet2_loop jeop (st : State2players.t) : int =
  print_endline ("Welcome to FINAL JEOPARDY!");
  print_string ("Player2, your current score is: ");
  print_endline (string_of_int (State2players.current_player2_score st));
  print_string ("Player1's score is: ");
  print_endline (string_of_int (State2players.current_player1_score st));
  print_endline ("You could win double the points you bet. Place your bet:");
  try (let bet = int_of_string (read_line()) in
       if bet < 0 then
         (print_endline 
            "Do you want to lose points?!!!!! Bet a positive number!";
          final_bet2_loop jeop st)
       else if bet > State2players.current_player2_score st then 
         (print_endline "You don't have that many points! Don't be greedy.";
          final_bet2_loop jeop st)
       else 
         (ANSITerminal.erase Screen;
          bet))
  with 
  | Failure (int_of_string) -> 
    print_endline 
      "That's an invalid input (bet needs to be a number)";
    final_bet2_loop jeop st 

(** [final_two_player_loop jeop st] prompts both player for their final
    jeopardy round bets and then their answers and adjusts the players' scores
    according to whether they got the question correct. *)
let final_two_player_loop jeop (st : State2players.t) =
  ANSITerminal.erase Screen;
  let bet1 = final_bet1_loop jeop st in
  let bet2 = final_bet2_loop jeop st in
  let bet_state = 
    match State2players.bet st bet1 bet2 with
    | Legal s -> s
    | Illegal -> print_endline "This is wrong"; st
  in
  let ans1 = final_answer1_loop jeop bet_state in
  let ans2 = final_answer2_loop jeop bet_state in
  match State2players.final_answer jeop bet_state ans1 ans2 with
  | Legal s -> s
  | Illegal -> print_endline "This is wrong"; st

(** [play_loop_two_player jeop st] is the same as [play_loop] but for two-player
    games. *)
let rec play_loop_two_player jeop (st : State2players.t) (skipping: bool) =
  if (List.length (State2players.current_categories st)) = 0 then
    (let score1 = State2players.current_player1_score st in
     let score2 = State2players.current_player2_score st in
     if not (State2players.has_played_final st) && (score1 < 0 || score2 < 0) 
     then 
       (print_endline ("You've finished the game!! Good job!");
        print_string ("The scores were: \nplayer 1: ");
        print_endline ((string_of_int score1));
        print_string ("player 2: ");
        print_endline ((string_of_int score2));
        print_endline ("Since you do not both have positive scores, there is "
                       ^ "no need for a final jeopardy round!");
        if score1 > score2 then print_endline ("Congratulations player 1!")
        else if score2 > score1 then print_endline ("Congratulations player 2!")
        else print_endline ("You tied! Good Game.");
        exit 0)
     else 
       (if State2players.has_played_final st then
          (let score1 = State2players.current_player1_score st in
           let score2 = State2players.current_player2_score st in
           print_endline ("You've finished the game!! Good job!");
           print_string ("The scores were: \nplayer 1: ");
           print_endline ((string_of_int score1));
           print_string ("player 2: ");
           print_endline ((string_of_int score2));
           if score1 > score2 then print_endline ("Congratulations player 1!")
           else if score2 > score1 then print_endline 
               ("Congratulations player 2!")
           else print_endline ("You tied! Good Game.");
           exit 0)
        else 
          play_loop_two_player jeop (final_two_player_loop jeop st) false))
  else
    (print_endline ("Here are the current categories and levels left:\n");
     let board = State2players.current_board st in
     ANSITerminal.(print_string [blue] (board ^ "\n"));
     if (State2players.get_current_player st = One) 
     then ANSITerminal.(print_string [red] "Player 1, ")
     else ANSITerminal.(print_string [red] "Player 2, ");
     print_string ("please choose a category: ");
     match parse (read_line ()) with
     | exception Empty -> (print_string "\nPlease choose an category!\n";
                           play_loop_two_player jeop st false)
     | exception Malformed -> 
       (print_string "\nThat's an invalid category score combination!\n";
        play_loop_two_player jeop st false)
     | comm -> match comm with
       | Play lst -> (* Check if question exists? *)
         (match lst with
          | cat::lev::[] -> 
            (try 
               (let levels = State2players.current_category_levels st 
                    (Jeopardy.category_name_from_string cat) in
                if List.mem (int_of_string lev) levels then
                  ((print_endline "OK, your question is ... \n";
                    print_endline (Jeopardy.question jeop 
                                     (Jeopardy.category_name_from_string cat) 
                                     (int_of_string lev));
                    let ques_result = 
                      question_loop_two_player jeop st (int_of_string lev) 
                        (Jeopardy.category_name_from_string cat) in
                    let result = ((                                                                              
                        State2players.play 
                          (Jeopardy.category_name_from_string cat) 
                          (int_of_string lev) jeop ques_result)) in          
                    (match result with
                     | Legal t -> play_loop_two_player jeop t false
                     | Illegal -> 
                       print_endline 
                         "That's not a valid category level combination.";
                       play_loop_two_player jeop st false)))
                else
                  (print_endline 
                     "This is not an available level. Try again. \n";
                   play_loop_two_player jeop st false))
             with | Jeopardy.UnknownCategory cat -> 
               (print_endline 
                  "That category doesn't exist/has already been played.";
                play_loop_two_player jeop st false)
                  | Failure (int_of_string) -> 
                    (print_endline 
                       "That's an invalid input (level needs to be a number)";
                     play_loop_two_player jeop st false))
          | _ -> (print_endline "You forgot to type a category or score";
                  play_loop_two_player jeop st false))
       | Answer lst -> 
         print_endline 
           "You need to choose a category right now, not answer a question.";
         play_loop_two_player jeop st false
       | Score -> print_endline "Very well, your score is: ";
         if State2players.get_current_player st = One then 
           print_endline 
             (string_of_int (State2players.current_player1_score st))
         else 
           print_endline 
             (string_of_int (State2players.current_player2_score st));
         play_loop_two_player jeop st false
       | Quit -> (print_endline "OK, see ya next time!"; exit 0)
       | Hint -> print_endline "You need to choose a category and level first!";
         play_loop_two_player jeop st false
       | Pass -> print_endline "You haven't even chosen a question yet!";
         play_loop_two_player jeop st false
       | Skip -> print_endline "You haven't even chosen a question yet!";
         play_loop_two_player jeop st false
       | Double -> print_endline "You haven't even chosen a question yet!";
         play_loop_two_player jeop st false)

(** [play_game f] starts the jeopardy in jeopardy [jeop]. *)
let rec play_game jeop =
  ANSITerminal.resize 165 60;
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
  ANSITerminal.(print_string [red] 
                  "Here are the rules of Jeopardy:

   You're allowed to play with either one or two players and answer trivia 
   questions in order to get the highest amount of points. 

   - For each turn the player can choose a question from the board by choosing 
   the category and point value. EG: 'play Music 300' 

   - You are then presented with a question, but you MUST answer it in the form 
     of a question ('who is' or 'what is', etc). 

   - If you answer the question correctly your score goes up by however 
     much the question was worth, if you get it wrong, you lose that many 
     points.

   - You can also 'pass' and recieve no points. But be careful, you only have 3 
     passes a game! 

   - You can also ask for a 'hint' for every question, but this costs 100 points
     each time. 

   - At any point you check your score with 'score' 
     or quit the game with 'quit'. 

   - You also have one chance to double the points gained or lost in a question,
     just enter 'double' while playing the question.

   - In two-player mode, each player also has the ability to skip the other's 
     turn once per game, enter 'skip' when answering a question. 

   Have fun and good luck!\n");

  ANSITerminal.(print_string [red] "\n 1 or 2 players?\n");
  match read_line() with
  | "One" | "1" | "one" -> 
    let st = State.init_state jeop in
    (play_loop jeop st)
  | "Two" | "2" | "two" -> 
    let st = State2players.init_state jeop in
    play_loop_two_player jeop st false
  | _ -> print_endline "You can only have one or two players";
    play_game jeop

(** [prompt_levs n] prompts the player for the number of levels per category
    the player wants to have in the game, with a minimum of 1 level and a
    maximum of [n] levels. *)
let rec prompt_levs (n: int) : int =
  print_string ("How many levels for each category do you want (max " 
                ^ string_of_int n ^ ")\n>");
  try (let s = read_int () in
       if s > n || s <= 0 then (
         ANSITerminal.erase Screen;
         print_endline "That's not a valid input";
         prompt_levs n) else s)
  with | Failure _ -> 
    ANSITerminal.erase Screen;
    print_endline "You need to input an int.";
    prompt_levs n

(** [main ()] prompts for what categories to play then starts it. *)
let rec main () =
  (* ANSITerminal.resize 140 40; *)
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our jeopardy game extravaganza!\n");
  print_endline "Here are the available categories: \n";
  let global_cats = Jeopardy.global_cats () in
  let () = List.fold_left (fun x y -> ()) () 
      (List.map (fun (x: Jeopardy.category_name) -> 
           print_endline (Jeopardy.category_name_string x)) global_cats) in
  print_endline 
    "Select with which categories you want to play (separated by spaces)";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | s -> (ANSITerminal.erase Screen;
          let split_list = Command.remove_empty 
              (String.split_on_char ' ' (String.lowercase_ascii s)) [] in
          let cats = List.map (Jeopardy.category_name_from_string) split_list in
          try (let jeop = Jeopardy.from_categories 
                   (Yojson.Basic.from_file "jeop.json") cats in
               let num_levs = prompt_levs (Jeopardy.get_lowest_level jeop) in
               play_game (Jeopardy.reduce num_levs jeop))
          with | Jeopardy.UnknownCategory _ ->
            ANSITerminal.erase Screen;
            print_endline "Sorry, one of those is not an available category.\n";
            main ()
               | Jeopardy.NoCategoriesProvided ->
                 ANSITerminal.erase Screen;
                 print_endline "You need to pick at least one category!\n";
                 main ())

(* Execute the game engine. *)
let () = main ()