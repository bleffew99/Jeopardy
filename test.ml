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
    (cat_name: category_name)
    (score: int)
    (expected_output: string list) : test =
  name >:: (fun _ -> 
      assert_equal expected_output (answers cat_name score))

let answer_unit cat_name score () = answers cat_name score

let make_answers_error_test 
    (name: string)
    (cat_name: category_name)
    (score: int)
    (expected_output: exn) : test =
  name >:: (fun _ -> 
      assert_raises (UnknownCategory cat_name) (answers_unit cat_name score))

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
    make_levels_test "level 2" (t2) (category_name_from_string "Music") [100; 200; 300; 400; 500];
    (*make_levels_test "level 3" (t2) (category_name_from_string "Quotes") [100; 200; 300; 400; 500];*)

    (*levels test with exception*)
    make_levels_error_test "level error 1" (t2) (category_name_from_string "Animal") 
      (UnknownCategory (category_name_from_string "Animal")) ; 
    make_levels_error_test "level error 2" (t1) (category_name_from_string "Animal") 
      (UnknownCategory (category_name_from_string "Animal")) ;                        

    (*question test without exception*)
    make_question_test "question 1" (t1) (category_name_from_string "CS3110") 100 "Who is the professor of CS3110?";
    make_question_test "question 2" (t2) (category_name_from_string "Music") 200 "Complete: 'Once I was seven years old my momma told me Go make ___ some friends' ";
    make_question_test "question 3" (t2) (category_name_from_string "Disney") 500 "Which country is the movie 'Frozen' set in?";

    (*question test with exceptions*)
    make_question_error_test "question error 1" (t1) (category_name_from_string "CS3110") (200) (UnknownLevel 200);
    make_question_error_test "question error 2" (t2) (category_name_from_string "Music") (727) (UnknownLevel 727);

    (*answers test no excpetion*)
    make_answers_test "answer test 1" (category_name_from_string "CS3110") 100 ["Michael Clarkson";
                                                                                "clarkson";"Clarkson"];
    make_answers_test "answer test 2" (category_name_from_string "Disney") 400 ["Sleeping Beauty";
                                                                                "sleeping beauty";"Sleeping beauty"];
    make_answers_test "answer test 3" (category_name_from_string "Music") 200 [ "yourself";
                                                                                "your self";"Yourself"];

    (*answers test exception*)
    make_answers_error_test "answer error 1" (category_name_from_string "Fruit") 400 
      (UnknownCategory (category_name_from_string "Fruit"));
  ]

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
    make_parse_error_test "empty" "" Malformed;


  ]

let suite =
  "test suite for midterm project"  >::: List.flatten [
    jeopardy_tests;
    command_tests
  ]

let _ = run_test_tt_main suite