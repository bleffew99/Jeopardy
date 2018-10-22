open OUnit2
open Jeopardy
open State

(* jeopardy tests*)

let make_get_category_name_test 
    (name : string) 
    (cat: category) 
    (expected_output : string) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (category_name_string (get_category_name cat)))

let cate = categories (from_json(Yojson.Basic.from_file "3110.json"))

let jeopardy_tests = 
  [
    (*get_category_name tests*)
    make_get_category_name_test "get category name 1"
      (List.nth (cate) 0) "CS3110"

  ]

let suite =
  "test suite for midterm project"  >::: List.flatten [
    jeopardy_tests;
  ]

let _ = run_test_tt_main suite