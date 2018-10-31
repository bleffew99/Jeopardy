open OUnit2
open Jeopardy
open Command
open State

(* jeopardy tests *)

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

let t1 = from_json(Yojson.Basic.from_file "3110.json")
let t2 = from_json(Yojson.Basic.from_file "jeop1.json")
let t3 = from_json(Yojson.Basic.from_file "jeop2.json")
let cate1 = categories (t1)
let cate2 = categories (t2)

let jeopardy_tests = 
  [
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

  ]

(* state tests*)
let make_current_score_test  
    (name: string)
    (st: State.t)
    (expected_output: int) : test =
  name >:: (fun _ ->
      assert_equal expected_output (current_score st))

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

let make_hint_illegal_tests
    (name : string)
    (cat : Jeopardy.category_name)
    (lev : int)
    (jeop : Jeopardy.t)
    (st : State.t)
    (expected_output : result) : test =
  name >:: (fun _ ->
      assert_equal expected_output (hint cat lev jeop st)) 

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

let make_state = function 
  | Legal t -> t
  | Illegal -> raise (Failure "Illegal")

let play0 = init_state t2
let play1 = make_state (play (category_name_from_string "Disney") 200 t2 play0) 
let play2 = make_state (play (category_name_from_string "Music") 300 t2 play1)
let play3 = make_state (play (category_name_from_string "Disney") 100 t2 play2)
let play4 = make_state (play (category_name_from_string "Disney") 400 t2 play3)
let play5 = make_state (play (category_name_from_string "Disney") 500 t2 play4)
let play6 = make_state (play (category_name_from_string "Disney") 300 t2 play5)

let ans1 = make_state (answer (category_name_from_string "Disney") 200 
                         "snow white" t2 play1) 
let ans2 = make_state (answer (category_name_from_string "Music") 300 
                         "despicable me 2" t2 ans1) 
let ans3 = make_state (answer (category_name_from_string "Disney") 100 
                         "simba" t2 ans2) 
let ans4 = make_state (answer (category_name_from_string "Disney") 400 
                         "sleeping beauty" t2 ans3) 
let ans5 = make_state (answer (category_name_from_string "Disney") 500 
                         "norway" t2 ans4) 
let ans6 = make_state (answer (category_name_from_string "Disney") 300 
                         "norway" t2 ans5) 

let hint2 = make_state (hint (category_name_from_string "Music") 300 t2 ans1)
let hintans2 = make_state (answer (category_name_from_string "Music") 300 
                             "despicable me 2" t2 hint2) 
let hint3 = make_state 
    (hint (category_name_from_string "Disney") 100 t2 hintans2)
let hintans3 = make_state (answer (category_name_from_string "Disney") 100 
                             "simba" t2 hint3) 

let state_tests = [
  (*current_score tests*)
  make_current_score_test "current score test 1" ans1 200;
  make_current_score_test "current score test 2" ans2 500;
  make_current_score_test "current score test 3" ans3 400;
  make_current_score_test "current score test 4" ans4 800;
  make_current_score_test "current score test 5" ans5 1300;
  make_current_score_test "current score test 6" ans6 1000; 
  (*to test hint score reduction*)
  make_current_score_test "current score test 7" hint2 100; 
  make_current_score_test "current score test 8" hintans2 400; 
  make_current_score_test "current score test 9" hint3 300; 
  make_current_score_test "current score test 10" hintans3 200; 

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
]
let make_state2 (res:State2players.result): State2players.t = 
match res with
  | Legal t -> t
  | Illegal -> raise (Failure "Illegal")
let play10 = State2players.init_state t3
let play11 = make_state2 (State2players.play (category_name_from_string "States"
) 100 t3 play10)
let play12 = make_state2 (State2players.play (category_name_from_string 
"Holidays") 400 t3 play11)
let play13 = make_state2 (State2players.play (category_name_from_string 
"Animals") 300 t3 play12)

let ans10 = make_state2 (State2players.answer (category_name_from_string 
"States") 100 "california" t3 play11)
let ans11 = make_state2 (State2players.answer (category_name_from_string 
"Holidays") 400 "jack o' lantern" t3 play12)
let ans12 = make_state2 (State2players.answer (category_name_from_string 
"Animals") 300 "jack o' lantern" t3 play13) (*player answers wrong*)
                                              
let make_current_player 
  (name: string)
  (st: State2players.t)
  (expected_output: State2players.current_player) :test =
  name >:: (fun _ ->
      assert_equal expected_output (State2players.get_current_player st))
let state_2_tests = [
  make_current_player "init" play10 One;
  make_current_player "tester1" ans10 Two;
  make_current_player "tester1" ans11 One;
  make_current_player "tester2" ans12 Two; 

]
let suite =
  "test suite for midterm project"  >::: List.flatten [
    jeopardy_tests;
    command_tests;
    state_tests;
    state_2_tests
  ]

let _ = run_test_tt_main suite