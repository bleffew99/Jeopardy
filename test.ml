open OUnit2
open Jeopardy
open Command
open State

(* jeopardy tests *)
let make_get_lowest_level_test 
    (name: string)
    (jeop : Jeopardy.t)
    (expected_output: int) :test =
  name >:: (fun _ ->  assert_equal expected_output 
               (Jeopardy.get_lowest_level jeop))

let make_get_category_name_test 
    (name : string) 
    (cat: category) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output 
        (category_name_string (get_category_name cat)))

let make_levels_test 
    (name: string) 
    (jeop: Jeopardy.t)
    (cat_name: category_name) 
    (expected_output : int list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (levels jeop cat_name))

let level_unit jeop cat_name () = levels jeop cat_name

let make_levels_error_test 
    (name: string)
    (jeop: Jeopardy.t)
    (cat_name: category_name)
    (expected_output: exn) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownCategory cat_name) (level_unit jeop cat_name))

let make_all_levels_test 
    (name : string) 
    (jeop: Jeopardy.t)
    (expected_output : int list list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (all_levels jeop))

let make_question_test 
    (name : string) 
    (jeop: Jeopardy.t)
    (cat: category_name)
    (score: int)
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (question jeop cat score))

let question_unit jeop cat score () = question jeop cat score

let make_question_error_test 
    (name: string)
    (jeop: Jeopardy.t)
    (cat: category_name)
    (score: int)
    (expected_output: exn) : test = 
  name >:: (fun _ -> 
      assert_raises (UnknownLevel score) (question_unit jeop cat score))

let make_answers_test 
    (name: string)
    (jeop: Jeopardy.t)
    (cat_name: category_name)
    (score: int)
    (expected_output: string list) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (answers jeop cat_name score))

let answer_unit jeop cat_name score () = answers jeop cat_name score

let make_answers_error_test 
    (name: string)
    (jeop: Jeopardy.t)
    (cat_name: category_name)
    (score: int)
    (expected_output: exn) : test =
  name >:: (fun _ -> 
      assert_raises (UnknownCategory cat_name) 
        (answer_unit jeop cat_name score))

let make_hint_test 
    (name: string)
    (cat : category_name)
    (score: int)
    (jeop: Jeopardy.t) 
    (expected_output : string) : test = 
  name >:: (fun _ ->  assert_equal expected_output 
               (Jeopardy.hint jeop cat score))

let hint_unit cat score jeop () = Jeopardy.hint jeop cat score

let make_hint_unknown_cat_test 
    (name: string)
    (cat : category_name)
    (score: int)
    (jeop: Jeopardy.t) 
    (expected_output : exn) : test = 
  name >::(fun _ -> assert_raises (UnknownCategory cat) 
              (hint_unit cat score jeop))

let make_hint_unknown_lev_test 
    (name: string)
    (cat : category_name)
    (score: int)
    (jeop: Jeopardy.t) 
    (expected_output : exn) : test = 
  name >:: (fun _ -> assert_raises (UnknownLevel score) 
               (hint_unit cat score jeop))

let make_final_jeopardy_question_test 
    (name: string)
    (jeop: Jeopardy.t) 
    (expected_output : string) : test = 
  name >:: (fun _ ->  assert_equal expected_output 
               (Jeopardy.final_jeopardy_question jeop))

let make_final_jeopardy_answers_test 
    (name: string)
    (jeop: Jeopardy.t) 
    (expected_output : string list) : test = 
  name >:: (fun _ ->  assert_equal expected_output 
               (Jeopardy.final_jeopardy_answers jeop))

let t1 = from_json(Yojson.Basic.from_file "3110.json")
let t2 = from_json(Yojson.Basic.from_file "jeop1.json")
let t3 = from_json(Yojson.Basic.from_file "jeop2.json")
let t4 = from_json(Yojson.Basic.from_file "jeop3.json")

let cate1 = categories (t1)
let cate2 = categories (t2)
let newt1 = from_categories (Yojson.Basic.from_file "jeop.json") 
    [(category_name_from_string "Music");(category_name_from_string "States")]
let newt2 = from_categories (Yojson.Basic.from_file "jeop.json") 
    [(category_name_from_string "CS");(category_name_from_string "Holidays");
     (category_name_from_string "Music")]
let newt3 = from_categories (Yojson.Basic.from_file "jeop.json") 
    [(category_name_from_string "CS");(category_name_from_string "Animals")]        
let redt1 = reduce 500 newt1 
let redt2 = reduce 400 newt2
let redt3 = reduce 400 newt3 

let jeopardy_tests = 
  [
    (*get lowest level tests*)
    make_get_lowest_level_test "low test 1" newt1 5;
    make_get_lowest_level_test "low test 2" newt2 4;
    make_get_lowest_level_test "low test 3" newt3 6;   

    (*get_category_name tests*)
    make_get_category_name_test "get category name 1"
      (List.nth (cate1) 0) "CS3110" ;
    make_get_category_name_test "get category name 2"
      (List.nth (cate2) 0) "Music"; 
    make_get_category_name_test "get category name 2"
      (List.nth (cate2) 3) "Disney";   

    (*levels test without exception*)
    make_levels_test "level 1" (t1) 
      (category_name_from_string "CS3110") [100];
    make_levels_test "level 2" (t2) 
      (category_name_from_string "Music") [100; 200; 300; 400; 500];
    make_levels_test "level 3" (t2) 
      (category_name_from_string "Quotes") 
      [100; 200; 300; 400; 500];

    (*levels test with exception*)
    make_levels_error_test "level error 1" (t2) (category_name_from_string 
                                                   "Animal") 
      (UnknownCategory (category_name_from_string "Animal")) ; 
    make_levels_error_test "level error 2" (t1) (category_name_from_string 
                                                   "Animal") 
      (UnknownCategory (category_name_from_string "Animal")) ;                        

    (*all_levels test*)
    make_all_levels_test "all levels 1" redt1 ([[100;200;300;400;500];[100;200;300;400;500]]);
    make_all_levels_test "all levels 2" redt2 ([[100;200;300;400];[100;200;300;400];[100;200;300;400]]);
    make_all_levels_test "all levels 3" redt3 ([[100;200;300;400];[100;200;300;400]]);

    (*question test without exception*)
    make_question_test "question 1" (t1) (category_name_from_string "CS3110") 
      100 "Who is the professor of CS3110?";
    make_question_test "question 2" (t2) (category_name_from_string "Music") 
      500 "This singer sings treat you better.";
    make_question_test "question 3" (t2) (category_name_from_string "Disney")
      500 "Which country is the movie 'Frozen' set in?";

    (*question test with exceptions*)
    make_question_error_test "question error 1" (t1) 
      (category_name_from_string "CS3110") (200) (UnknownLevel 200);
    make_question_error_test "question error 2" (t2) 
      (category_name_from_string "Music") (727) (UnknownLevel 727);

    (*answers test no excpetion*)
    make_answers_test "answer test 1" t1 (category_name_from_string "CS3110") 
      100 ["clarkson";"michael clarkson";"michael"];
    make_answers_test "answer test 2" t2 (category_name_from_string "Disney") 
      400 ["sleeping beauty"];
    make_answers_test "answer test 3" t2 (category_name_from_string "Music") 
      200 [ "yourself"];

    (*answers test exception*)
    make_answers_error_test "answer error 1" t2 
      (category_name_from_string "Fruit") 400 
      (UnknownCategory (category_name_from_string "Fruit"));
    make_answers_error_test "answer error 1" t2 
      (category_name_from_string "School") 
      1000 (UnknownCategory (category_name_from_string "Fruit"));

    (*hint test no exception*)
    make_hint_test "hint test 1" (category_name_from_string "States") (500) (t3)
      "this midwestern state's capitol is Lincoln.";
    make_hint_test "hint test 2"(category_name_from_string "Classical")(300)(t3)
      "A favorite Christmas time ballet.";
    make_hint_test "hint test 3" (category_name_from_string "Disney")(200)(t2)
      "Who bites the poisoned apple?";

    (*hint test exceptions*)
    make_hint_unknown_cat_test "hint error 1"(category_name_from_string "Apple") 
      500 t2 (UnknownCategory (category_name_from_string "Apple"));
    make_hint_unknown_cat_test "hint error 2"(category_name_from_string "Candy") 
      200 t3 (UnknownCategory (category_name_from_string "Candy"));
    make_hint_unknown_lev_test "hint error 3" 
      (category_name_from_string "Quotes") 600 t2 (UnknownLevel 600);
    make_hint_unknown_lev_test "hint error 4" (
      category_name_from_string "Animals") 900 t3 (UnknownLevel 900);  

    (*final question tests*)
    make_final_jeopardy_question_test "jeop 1" t2 
      ("Shakespeare uses the words moon and moonlight more than any other of his"^
       " works.");
    make_final_jeopardy_question_test "jeop 2" t3 ("It's the largest country"^
                                                   " in the world without any permanent rivers or lakes.");
    make_final_jeopardy_question_test "jeop3" t4 ("This 20th century"^
                                                  " Russian-American author, famous for such works as Lolita, Pnin, and"^
                                                  " Pale fire, also taught at Cornell for a time");

    (*final answers tests*)
    make_final_jeopardy_answers_test "jeop 1 ans" t2 ["a midsummer night's dream";
                                                      "midsummer nights dream"; "midsummer nights dream";"midsummer's nights dream";
                                                      "midsummers night dream"; "a midsummer nights dream"; "a midsummer nights dream";
                                                      "a midsummers nights dream";"a midsummers night dream";"midsummer night's dream";
                                                      "midsummer night's dream";"midsummer's night's dream";"a midsummer night's dream";
                                                      "a midsummer night's dream";"a midsummers night's dream"];
    make_final_jeopardy_answers_test "jeop 2 ans" t3 ["saudi arabia"; "saudi"];
    make_final_jeopardy_answers_test "jeop 3 ans" t4 ["vladimir nabokov";"nabokov";
                                                      "nobokov";"nabokoff";"nabakov";"nabokof"]; 

  ]

(* command tests*)

let make_parse_test
    (name : string)
    (str : string)
    (expected_output : command) : test =
  name >:: (fun _ ->
      assert_equal expected_output (parse str))
let parse_unit str () = parse str

let make_parse_empty_error_test 
    (name: string)
    (str : string)
    (expected_output : exn) : test = name >:: (fun _ -> assert_raises 
                                                  (Empty) (parse_unit str))

let make_parse_error_test 
    (name: string)
    (str : string)
    (expected_output : exn) : test = name >:: (fun _ -> assert_raises 
                                                  (Malformed) (parse_unit str))

let command_tests = 
  [
    (* make_parse test*)
    make_parse_test "parse play 1" "play Music 200" (Play ["Music";"200"]);
    make_parse_test "parse play 2" "play Cornell 400" (Play ["Cornell";"400"]); 
    make_parse_test "parse Answer" "what are apples" (Answer ["apples"]);
    make_parse_test "parse Score" "score" (Score);
    make_parse_test "parse Quit" "quit" (Quit);
    make_parse_test "parse Hint" "hint" (Hint);
    make_parse_test "parse Pass" "pass" (Pass);
    make_parse_test "parse Skip" "skip" (Skip);
    make_parse_test "parse Double" "double" (Double);    
    make_parse_empty_error_test "Empty" "" Empty;
    make_parse_error_test "Malformed play 1" "play" Malformed;
    make_parse_error_test "Malformed play 2" "play Fruits" Malformed;
    make_parse_error_test "Malformed play 3" "play bla bla bla" Malformed;
    make_parse_error_test "Malformed quit 1" "quit hello" Malformed;
    make_parse_error_test "Malformed score 1" "score yay" Malformed;
    make_parse_error_test "Malformed answer 1" "shawn" Malformed;
    make_parse_error_test "Malformed answer 2" "what is" Malformed;
    make_parse_error_test "Malformed answer 3" "who" Malformed;
    make_parse_error_test "Malformed hint 1" "hint yes" Malformed;
    make_parse_error_test "Malformed pass 1" "pass no" Malformed;
    make_parse_error_test "Malformed skip 1" "skip 4543" Malformed;    
    make_parse_error_test "Malformed double 1" "double hello" Malformed;
  ]

(* state tests*)
let make_current_score_test  
    (name: string)
    (st: State.t)
    (expected_output: int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (current_score st))

let make_current_passes_left_test  
    (name: string)
    (st: State.t)
    (expected_output: int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (current_passes_left st))

let make_get_final_bet_test  
    (name: string)
    (st: State.t)
    (expected_output: int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (get_final_bet st))            

let make_has_played_final_tests 
    (name: string)
    (st : State.t)
    (expected_output: bool):test = 
  name >:: (fun _ -> 
      assert_equal expected_output (has_played_final st))

let make_has_used_double_tests 
    (name: string)
    (st : State.t)
    (expected_output: bool):test = 
  name >:: (fun _ -> 
      assert_equal expected_output (has_used_double st))

let make_current_category_levels_test  
    (name: string)
    (state: State.t)
    (cate_name: category_name)
    (expected_output: int list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (current_category_levels state cate_name))

let make_play_illegal_tests
    (name : string)
    (cat : category_name)
    (lev : int)
    (jeop : Jeopardy.t)
    (st : State.t)
    (expected_output : result) : test =
  name >:: (fun _ ->
      assert_equal expected_output (play cat lev jeop st))

let make_answer_illegal_tests
    (name : string)
    (cat : Jeopardy.category_name)
    (lev : int)
    (ans : string)
    (jeop : Jeopardy.t)
    (st : State.t)
    (expected_output : result) : test =
  name >:: (fun _ ->
      assert_equal expected_output (answer cat lev ans jeop st))

let make_hint_illegal_tests
    (name : string)
    (cat : Jeopardy.category_name)
    (lev : int)
    (jeop : Jeopardy.t)
    (st : State.t)
    (expected_output : result) : test =
  name >:: (fun _ ->
      assert_equal expected_output (hint cat lev jeop st))

let make_pass_illegal_tests
    (name : string)
    (st : State.t)
    (expected_output : result) : test =
  name >:: (fun _ ->
      assert_equal expected_output (pass st))

let make_double_illegal_tests
    (name: string)
    (st: State.t)
    (jeop: Jeopardy.t)
    (cat: Jeopardy.category_name)
    (lev: int)
    (ans: string)
    (expected_output:result):test = 
  name >:: (fun _ -> 
      assert_equal expected_output (double st jeop cat lev ans))

let make_state = function 
  | Legal t -> t
  | Illegal -> raise (Failure "Illegal")

let play0 = init_state t2
let ans1 = make_state (answer (category_name_from_string "Disney") 200 
                         "snow white" t2 play0) 
let play1 = make_state (play (category_name_from_string "Disney") 200 t2 ans1)
let ans2 = make_state (answer (category_name_from_string "Music") 300 
                         "despicable me 2" t2 play1)  
let play2 = make_state (play (category_name_from_string "Music") 300 t2 ans2)
let ans3 = make_state (answer (category_name_from_string "Disney") 100 
                         "simba" t2 play2) 
let play3 = make_state (play (category_name_from_string "Disney") 100 t2 ans3)
let ans4 = make_state (answer (category_name_from_string "Disney") 400 
                         "sleeping beauty" t2 play3) 
let play4 = make_state (play (category_name_from_string "Disney") 400 t2 ans4)
let ans5 = make_state (answer (category_name_from_string "Disney") 500 
                         "norway" t2 play4) 
let play5 = make_state (play (category_name_from_string "Disney") 500 t2 ans5)
let ans6 = make_state (answer (category_name_from_string "Disney") 300 
                         "norway" t2 play5) 
let play6 = make_state (play (category_name_from_string "Disney") 300 t2 ans6)


let hint2 = make_state (hint (category_name_from_string "Music") 300 t2 play1)
let hintans2 = make_state (answer (category_name_from_string "Music") 300 
                             "despicable me 2" t2 hint2) 
let hintplay2 = make_state (play (category_name_from_string "Music") 300 
                              t2 hintans2)
let hint3 = make_state 
    (hint (category_name_from_string "Disney") 100 t2 hintplay2)
let hintans3 = make_state (answer (category_name_from_string "Disney") 100 
                             "simba" t2 hint3) 
let hintplay3 = make_state (play (category_name_from_string "Disney") 100 
                              t2 hintans3)

let pass2 = make_state (pass play1)
let pass3 = make_state (pass pass2)
let passans4 = make_state (answer (category_name_from_string "Disney") 400 
                             "sleeping beauty" t2 pass3) 
let passplay4 = make_state (play (category_name_from_string "Disney") 400 
                              t2 passans4)
let pass5 = make_state (pass passplay4)

let bet1 = make_state (bet play1 400)
let finans1 = make_state (final_answer t2 bet1 "a midsummer night's dream")
let bet2 = make_state (bet play3 200)
let finans2 = make_state (final_answer t4 bet2 "vladimir nabokov")
let bet3 = make_state (bet play4 800)
let finans3 = make_state (final_answer t3 bet3 "wrong answer")

let double1 = make_state (double hintans2 t3
                            (category_name_from_string "Animals") 
                            300 "platypus")
let double2 = make_state (double ans5 t4(category_name_from_string "Flags") 
                            300 "circle")
let double3 = make_state (double ans6 t2(category_name_from_string "Cornell") 
                            500 "wrong")                           

let state_tests = [
  (*current_score tests*)
  make_current_score_test "current score test 1" ans1 200;
  make_current_score_test "current score test 2" ans2 500;
  make_current_score_test "current score test 3" ans3 400;
  make_current_score_test "current score test 4" ans4 800;
  make_current_score_test "current score test 5" ans5 1300;
  make_current_score_test "current score test 6" ans6 1000;

  (*test hint score reduction*)
  make_current_score_test "current score test 7" hint2 100; 
  make_current_score_test "current score test 8" hintans2 400; 
  make_current_score_test "current score test 9" hint3 300; 
  make_current_score_test "current score test 10" hintans3 200; 

  (*test pass score reduction*)
  make_current_score_test "current score test 11" pass2 200; 
  make_current_score_test "current score test 12" pass3 200; 
  make_current_score_test "current score test 13" passans4 600; 
  make_current_score_test "current score test 14" passplay4 600; 
  make_current_score_test "current score test 15" pass5 600; 

  (*test bet score*)
  make_current_score_test "current score test 16" finans1 600;
  make_current_score_test "current score test 17" finans2 600;
  make_current_score_test "current score test 18" finans3 0;

  (*test double score*)
  make_current_score_test "current score test 19" double1 1000;
  make_current_score_test "current score test 20" double2 1900;
  make_current_score_test "current score test 21" double3 0;

  (*current passes left test*)  
  make_current_passes_left_test "current passes left 1" play1 3;
  make_current_passes_left_test "current passes left 2" pass2 2;
  make_current_passes_left_test "current passes left 3" pass3 1;
  make_current_passes_left_test "current passes left 4" passplay4 1;
  make_current_passes_left_test "current passes left 5" pass5 0;

  (*get final bet test*) 
  make_get_final_bet_test "get final bet 1" bet1 400;
  make_get_final_bet_test "get final bet 1" bet2 200;
  make_get_final_bet_test "get final bet 1" bet3 800;

  (*has_played_final tests for bet*)
  make_has_played_final_tests "has played 1" bet1 false;
  make_has_played_final_tests "has played 2" finans1 true;
  make_has_played_final_tests "has played 3" finans3 true;

  (*has used double tests*)
  make_has_used_double_tests "has used 1" double1 true;
  make_has_used_double_tests "has used 2" play6 false;
  make_has_used_double_tests "has used 2" double3 true;

  (*current_category_levels tests*)
  make_current_category_levels_test "ccl test 1" play1 
    (category_name_from_string "Disney") [100; 300; 400; 500];
  make_current_category_levels_test "ccl test 2" play0
    (category_name_from_string "Quotes") [100; 200; 300; 400; 500];
  make_current_category_levels_test "ccl test 3" play2 
    (category_name_from_string "Music") [100; 200; 400; 500];
  make_current_category_levels_test "ccl test 4" play4 
    (category_name_from_string "Disney") [300; 500];

  (*play illegal tests*)
  make_play_illegal_tests "play illegal test 1" 
    (category_name_from_string "Disney") 100 t2 play6 (Illegal);
  make_play_illegal_tests "play illegal test 2" 
    (category_name_from_string "Music") 300 t2 play6 (Illegal);

  (*answer illegal tests*)
  make_answer_illegal_tests "answer illegal test 1" 
    (category_name_from_string "Movies") 100 "hello" t2 play6 (Illegal);

  (*hint illegal tests*)
  make_hint_illegal_tests "hint illegal test 1" 
    (category_name_from_string "Movies") 100 t2 play6 (Illegal);
  make_hint_illegal_tests "hint illegal test 2" 
    (category_name_from_string "Music") 9878 t2 play6 (Illegal);

  (*pass illegal tests*) 
  make_pass_illegal_tests "pass illegal test 1" pass5 (Illegal);

  (*double illegal tests*)
  make_double_illegal_tests "double illegal test 1" double1 t2 
    (category_name_from_string "Music") (300) ("Despecable Me") Illegal;
  make_double_illegal_tests "double illegal test 2" double2 t2 
    (category_name_from_string "Disney") (100) ("the lion king") Illegal;  
]    


(*state2players tests*)
let make_state2 (res:State2players.result): State2players.t = 
  match res with
  | Legal t -> t
  | Illegal -> raise (Failure "Illegal")

let play10 = State2players.init_state t3
let ans11 = make_state2 (State2players.answer 
                           (category_name_from_string "States") 
                           100 "california" t3 play10)
let play11 = make_state2 (State2players.play 
                            (category_name_from_string "States") 
                            100 t3 ans11)
let ans12 = make_state2 (State2players.answer 
                           (category_name_from_string "Holidays") 
                           400 "jack o'lantern" t3 play11)
let play12 = make_state2 (State2players.play 
                            (category_name_from_string "Holidays") 
                            400 t3 ans12)
let ans13 = make_state2 (State2players.answer 
                           (category_name_from_string "Animals") 
                           300 "penguin" t3 play12) 
let play13 = make_state2 (State2players.play 
                            (category_name_from_string "Animals") 
                            300 t3 ans13)

let hint12 = make_state2 (State2players.hint 
                            (category_name_from_string "Holidays") 
                            400 t3 play11)
let hintans12 = make_state2 (State2players.answer 
                               (category_name_from_string "Holidays") 
                               400 "jack o'lantern" t3 hint12) 
let hintplay12 = make_state2 (State2players.play 
                                (category_name_from_string "Holidays") 
                                400 t3 hintans12)
let hint13 = make_state2 (State2players.hint 
                            (category_name_from_string "Animals") 
                            300 t3 hintplay12)
let hintans13 = make_state2 (State2players.answer 
                               (category_name_from_string "Animals") 
                               300 "penguin" t3 hint13) 
let hintplay13 = make_state2 (State2players.play 
                                (category_name_from_string "Animals") 
                                300 t3 hintans13)

let pass11 = make_state2 (State2players.pass play11)
let passplay11 = make_state2 (State2players.play 
                                (category_name_from_string "Animals") 
                                100 t3 pass11)
let pass12 = make_state2 (State2players.pass passplay11)
let passplay12 = make_state2 (State2players.play 
                                (category_name_from_string "Holidays") 
                                400 t3 pass12)     
let pass13 = make_state2 (State2players.pass passplay12)
let passplay13 = make_state2 (State2players.play 
                                (category_name_from_string "Animals") 
                                300 t3 pass13)                                                       

let bet10 = make_state2 (State2players.bet ans12 100 200)
let finans10 = make_state2 (State2players.final_answer t2 bet10 
                              "a midsummer night's dream" "wrong")
let bet11 = make_state2 (State2players.bet ans11 50 0)
let finans11 = make_state2 (State2players.final_answer t4 bet11 
                              "wrong" "vladimir nabokov")

let double11 = make_state2 (State2players.double play11 t3    
                              (category_name_from_string("Holidays")) 400 
                              "jack o'lantern")
let doubleplay12 = make_state2 (State2players.play 
                                  (category_name_from_string "Holidays") 
                                  400 t3 double11)
let double12 = make_state2 (State2players.double doubleplay12 t3 
                              (category_name_from_string("Animals")) 300 
                              "penguin")
let doubleplay13 = make_state2 (State2players.play 
                                  (category_name_from_string "Animals") 
                                  300 t3 double12)

let make_current_player_tests
    (name: string)
    (st: State2players.t)
    (expected_output: State2players.current_player) :test =
  name >:: (fun _ ->
      assert_equal expected_output (State2players.get_current_player st))

let make_current_player1_score_tests 
    (name: string)
    (st: State2players.t)
    (expected_output: int) :test =
  name >:: (fun _ ->
      assert_equal expected_output (State2players.current_player1_score st))

let make_current_player2_score_tests 
    (name: string)
    (st: State2players.t)
    (expected_output: int) :test =
  name >:: (fun _ ->
      assert_equal expected_output (State2players.current_player2_score st))

let state2players_tests = [

  (*current player 1 tests*)
  make_current_player1_score_tests "current player1 score test 1" play10 0;
  make_current_player1_score_tests "current player1 score test 2" ans11 100;
  make_current_player1_score_tests "current player1 score test 3" play11 100;
  make_current_player1_score_tests "current player1 score test 4" ans12 100;
  make_current_player1_score_tests "current player1 score test 5" play12 100;
  make_current_player1_score_tests "current player1 score test 6" ans13 (-200);
  make_current_player1_score_tests "current player1 score test 7" play13 (-200);
  make_current_player1_score_tests 
    "current player1 score test 8" hint12 100;
  make_current_player1_score_tests 
    "current player1 score test 9" hintans12 100;
  make_current_player1_score_tests 
    "current player1 score test 10" hintplay12 100;
  make_current_player1_score_tests 
    "current player1 score test 11" hint13 0;
  make_current_player1_score_tests 
    "current player1 score test 12" hintans13 (-300);
  make_current_player1_score_tests 
    "current player1 score test 13" hintplay13 (-300);
  make_current_player1_score_tests "bet 1" (finans10) (200);
  make_current_player1_score_tests "bet 2" (finans11) (50);
  make_current_player1_score_tests "double 1" (double11) (100);
  make_current_player1_score_tests "double 2" (doubleplay12) (100);
  make_current_player1_score_tests "double 3" (double12) (-500);
  make_current_player1_score_tests "double 4" (doubleplay13) (-500);
  make_current_player1_score_tests "pass 1 1" (pass11) (100);
  make_current_player1_score_tests "pass 1 2" (passplay11) (100);  

  (*player 2 score tests*)
  make_current_player2_score_tests "current player2 score test 1" play10 0;
  make_current_player2_score_tests "current player2 score test 2" ans11 0;
  make_current_player2_score_tests "current player2 score test 3" play11 0;
  make_current_player2_score_tests "current player2 score test 4" ans12 400;
  make_current_player2_score_tests "current player2 score test 5" play12 400;
  make_current_player2_score_tests "current player2 score test 6" ans13 400;
  make_current_player2_score_tests "current player2 score test 7" play13 400;
  make_current_player2_score_tests 
    "current player2 score test 8" hint12 (-100);
  make_current_player2_score_tests 
    "current player2 score test 9" hintans12 300;
  make_current_player2_score_tests 
    "current player2 score test 10" hintplay12 300;
  make_current_player2_score_tests 
    "current player2 score test 11" hint13 300;
  make_current_player2_score_tests 
    "current player2 score test 12" hintans13 300;
  make_current_player2_score_tests 
    "current player2 score test 13" hintplay13 300;
  make_current_player2_score_tests "bet player 2 1" (finans10) (200);
  make_current_player2_score_tests "bet player 2 2" (finans11) (0);
  make_current_player2_score_tests "double 2 1" (double11) (800);
  make_current_player2_score_tests "double 2 2" (doubleplay12) (800);
  make_current_player2_score_tests "double 2 3" (double12) (800);
  make_current_player2_score_tests "double 2 4" (doubleplay13) (800);
  make_current_player2_score_tests "pass 2 1" (pass11) (0);
  make_current_player2_score_tests "pass 2 2" (pass12) (0);
  make_current_player2_score_tests "pass 2 3" (passplay12) (0);
  make_current_player2_score_tests "pass 2 4" (passplay13) (0);

  (*current player tests*)
  make_current_player_tests "current player test 1" play10 One;
  make_current_player_tests "current player test 2" ans11 One;
  make_current_player_tests "current player test 3" play11 Two;
  make_current_player_tests "current player test 4" ans12 Two;
  make_current_player_tests "current player test 5" play12 One;
  make_current_player_tests "current player test 6" ans13 One; 
  make_current_player_tests "current player test 7" play13 Two;
  make_current_player_tests "current player test 8" hint12 Two; 
  make_current_player_tests "current player test 9" hintans12 Two; 
  make_current_player_tests "current player test 10" hintplay12 One; 
  make_current_player_tests "current player test 11" hint13 One; 
  make_current_player_tests "current player test 12" hintans13 One; 
  make_current_player_tests "current player test 13" hintplay13 Two; 
  make_current_player_tests "current player test 14" double11 Two; 
  make_current_player_tests "current player test 15" doubleplay12 One; 
  make_current_player_tests "current player test 16" double12 One; 
  make_current_player_tests "current player test 17" pass11 Two; 
  make_current_player_tests "current player test 18" passplay11 One; 
  make_current_player_tests "current player test 19" pass12 One; 
  make_current_player_tests "current player test 20" passplay12 Two; 
  make_current_player_tests "current player test 21" pass13 Two; 
  make_current_player_tests "current player test 22" passplay13 One; 
]

let suite =
  "test suite for midterm project"  >::: List.flatten [
    jeopardy_tests;
    command_tests;
    state_tests;
    state2players_tests
  ]

let _ = run_test_tt_main suite