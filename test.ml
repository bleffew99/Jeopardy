open OUnit2
open Jeopardy
open Command
open State

(* jeopardy tests*)

let make_get_category_name_test 
    (name : string) 
    (cat: category) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (category_name_string (get_category_name cat)))

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
      assert_raises (UnknownCategory cat_name) (answer_unit jeop cat_name score))

let t1 = from_json(Yojson.Basic.from_file "3110.json")
let t2 = from_json(Yojson.Basic.from_file "jeop1.json")
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
    make_levels_test "level 1" (t1) (category_name_from_string "CS3110") [100];
    make_levels_test "level 2" (t2) (category_name_from_string "Music") [100;
                                                                         200; 300; 400; 500];
    make_levels_test "level 3" (t2) (category_name_from_string "Quotes") 
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
      500 "This singer sings treat you better";
    make_question_test "question 3" (t2) (category_name_from_string "Disney")
      500 "Which country is the movie 'Frozen' set in?";

    (*question test with exceptions*)
    make_question_error_test "question error 1" (t1) (category_name_from_string 
                                                        "CS3110") (200) (UnknownLevel 200);
    make_question_error_test "question error 2" (t2) (category_name_from_string 
                                                        "Music") (727) (UnknownLevel 727);

    (*answers test no excpetion*)
    make_answers_test "answer test 1" t1 (category_name_from_string "CS3110") 
      100 ["Michael Clarkson";"clarkson";"Clarkson"];
    make_answers_test "answer test 2" t2 (category_name_from_string "Disney") 
      400 ["Sleeping Beauty"; "sleeping beauty";"Sleeping beauty"];
    make_answers_test "answer test 3" t2 (category_name_from_string "Music") 200 
      [ "yourself"; "your self";"Yourself"];

    (*answers test exception*)
    make_answers_error_test "answer error 1" t2 (category_name_from_string 
                                                   "Fruit") 400 (UnknownCategory (category_name_from_string "Fruit"));
    make_answers_error_test "answer error 1" t2 (category_name_from_string 
                                                   "School") 1000 (UnknownCategory (category_name_from_string "Fruit"));
  ]

(* command tests*)

let make_parse_test
    (name : string)
    (str : string)
    (expected_output : command) : test =
  name >:: (fun _ ->
      assert_equal expected_output (parse str))

let parse_unit str () = parse str

let make_parse_error_test
    (name : string)
    (str : string)
    (expected_output : exn) : test =
  name >:: (fun _ ->
      assert_raises expected_output (parse_unit str))

let command_tests = 
  [
    (* make_parse test*)
    make_parse_test "parse play 1" "play Music 200" (Play ["Music";"200"]);
    make_parse_test "parse play 2" "play Cornell 400" (Play ["Cornell";"400"]); 
    make_parse_test "parse Answer" "what are apples" (Answer ["apples"]);
    make_parse_test "parse Score" "score" (Score);
    make_parse_test "parse Quit" "quit" (Quit);
    (* make_parse_error_test "empty" "" Malformed;*)
    make_parse_error_test "Empty " "" Empty;
    make_parse_error_test "Malformed play 1" "play" Malformed;
    make_parse_error_test "Malformed play 2" "play Fruits" Malformed;
    make_parse_error_test "Malformed play 3" "play bla bla bla" Malformed;
    make_parse_error_test "Malformed quit 1" "quit hello" Malformed;
    make_parse_error_test "Malformed score 1" "score yay" Malformed;
    make_parse_error_test "Malformed answer 1" "shawn" Malformed;
    make_parse_error_test "Malformed answer 2" "what is" Malformed;
    make_parse_error_test "Malformed answer 3" "who" Malformed;
  ]

(* state tests*)

let make_current_category_levels_test  
    (name: string)
    (state: State.t)
    (cate_name: category_name)
    (expected_output: int list) : test =
  name >:: (fun _ ->
      assert_equal expected_output (current_category_levels state cate_name))

let make_current_category_levels_to_string_test  
    (name: string)
    (state: State.t)
    (expected_output: string) : test =
  name >:: (fun _ ->
      assert_equal expected_output (current_category_levels_to_string state)~printer:(fun (x:string) -> x))

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

let state_tests =
  [
    (*current_category_levels tests*)
    make_current_category_levels_test "ccl test 1" play1 
      (category_name_from_string "Disney") [100; 300; 400; 500];
    make_current_category_levels_test "ccl test 2" play0
      (category_name_from_string "Quotes") [100; 200; 300; 400; 500];
    make_current_category_levels_test "ccl test 3" play2 
      (category_name_from_string "Music") [100; 200; 400; 500];
    make_current_category_levels_test "ccl test 4" play4 
      (category_name_from_string "Disney") [300; 500];

    (*current_category_levels_to_string test*)
    make_current_category_levels_to_string_test "ccls test 1" play5
      ("Disney: 300" ^ "\n" ^ "Quotes: 100,200,300,400,500" ^ "\n" ^ 
       "Cornell: 100,200,300,400,500" ^ "\n" ^ 
       "Music: 100,200,400,500" ^ "\n");
    make_current_category_levels_to_string_test "ccls test 2" play6 
      ("Quotes: 100,200,300,400,500" ^ "\n" ^ "Cornell: 100,200,300,400,500" 
       ^ "\n" ^ "Music: 100,200,400,500" ^ "\n");

    (*play illegal tests*)
    make_play_illegal_tests "play illegal test 1" 
      (category_name_from_string "Disney") 100 t2 play6 (Illegal);
    make_play_illegal_tests "play illegal test 2" 
      (category_name_from_string "Music") 300 t2 play6 (Illegal);

    (*answer illegal tests*)
    make_answer_illegal_tests "answer illegal test 1" 
      (category_name_from_string "Movies") 100 "hello" t2 play6 (Illegal);

  ]

let suite =
  "test suite for midterm project"  >::: List.flatten [
    jeopardy_tests;
    command_tests;
    state_tests
  ]


let _ = run_test_tt_main suite