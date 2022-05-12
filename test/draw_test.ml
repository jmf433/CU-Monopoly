open OUnit2
open Final_project
open Draw

let first_element = function
  | a, _ -> a

let second_element = function
  | _, b -> b

let make_draw_cc_test
    (name : string)
    (t : Draw.t)
    (expected_output : t * card) =
  name >:: fun _ ->
  assert_equal expected_output (draw_community_chest t)

let make_draw_cc_test2
    (name : string)
    (t : Draw.t)
    (expected_output : card) =
  name >:: fun _ ->
  assert_equal expected_output (draw_community_chest t |> second_element)

let make_cards_left_test (name : string) (t : Draw.t) expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (t |> draw_community_chest |> first_element |> cc_cards_left)

let make_card_exists_test
    (name : string)
    (t : Draw.t)
    (id : int)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (cc_card_exists (t |> draw_community_chest |> first_element) id)

let make_card_exists_test2
    (name : string)
    (t : Draw.t)
    (id : int)
    (expected_output : bool) =
  name >:: fun _ -> assert_equal expected_output (cc_card_exists t id)

let make_card_in_map_test
    (name : string)
    (t : Draw.t)
    (id : int)
    (expected_output : cc_card) =
  name >:: fun _ -> assert_equal expected_output (cc_card_inmap t id)

let test_cc_json =
  from_json (Yojson.Basic.from_file "test_jsons/cc_test.json")

let test_cc_json2 =
  from_json (Yojson.Basic.from_file "test_jsons/default_card1.json")

let test_cc_json3 =
  from_json (Yojson.Basic.from_file "test_jsons/default_card2.json")

let draw_cc_tests =
  [
    (* make_draw_cc_test "Testing a draw from a sample of length 5 CC
       json" test_cc_json ( from_json (Yojson.Basic.from_file
       "test_jsons/cc_test2.json"), { description = "Advance to Go
       (Collect 200 BRB's)"; action = MoveToAndOwe (0, 200); } ); *)
    make_draw_cc_test2
      "Testing a draw from a sample of length 16 CC json" test_cc_json2
      {
        description = "Advance to Go (Collect 200 BRB's)";
        action = MoveToAndOwe (0, 200);
      };
    (* make_draw_cc_test "Testing a draw from a sample of length 5 CC
       json" test_cc_json ( from_json (Yojson.Basic.from_file
       "test_jsons/cc_test3.json"), { description = "Bank error in your
       favor. Collect $200"; action = MoneyChange 200; } ); *)
  ]

let cards_left_tests =
  [
    make_cards_left_test
      "Testing the default_card1.json file to see if the cardinality \
       works"
      test_cc_json2 15;
  ]

let card_exists_tests =
  [
    make_card_exists_test "Testing to see what ids remain in the list 1"
      test_cc_json2 1 false;
    make_card_exists_test "Testing to see what ids remain in the list 2"
      test_cc_json2 2 true;
    make_card_exists_test "Testing to see what ids remain in the list 3"
      test_cc_json2 3 true;
    make_card_exists_test "Testing to see what ids remain in the list 4"
      test_cc_json2 4 true;
    make_card_exists_test "Testing to see what ids remain in the list 5"
      test_cc_json2 5 true;
    make_card_exists_test "Testing to see what ids remain in the list 6"
      test_cc_json2 6 true;
    make_card_exists_test "Testing to see what ids remain in the list 7"
      test_cc_json2 7 true;
    make_card_exists_test "Testing to see what ids remain in the list 8"
      test_cc_json2 8 true;
    make_card_exists_test "Testing to see what ids remain in the list 9"
      test_cc_json2 9 true;
    make_card_exists_test
      "Testing to see what ids remain in the list 10" test_cc_json2 10
      true;
    make_card_exists_test
      "Testing to see what ids remain in the list 11" test_cc_json2 11
      true;
    make_card_exists_test
      "Testing to see what ids remain in the list 12" test_cc_json2 12
      true;
    make_card_exists_test
      "Testing to see what ids remain in the list 13" test_cc_json2 13
      true;
    make_card_exists_test
      "Testing to see what ids remain in the list 14" test_cc_json2 14
      true;
    make_card_exists_test
      "Testing to see what ids remain in the list 15" test_cc_json2 15
      true;
    make_card_exists_test
      "Testing to see what ids remain in the list 16" test_cc_json2 16
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 1" test_cc_json3 1
      false;
    make_card_exists_test2
      "Testing to see what ids remain in the list 2" test_cc_json3 2
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 3" test_cc_json3 3
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 4" test_cc_json3 4
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 5" test_cc_json3 5
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 6" test_cc_json3 6
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 7" test_cc_json3 7
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 8" test_cc_json3 8
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 9" test_cc_json3 9
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 10" test_cc_json3 10
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 11" test_cc_json3 11
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 12" test_cc_json3 12
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 13" test_cc_json3 13
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 14" test_cc_json3 14
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 15" test_cc_json3 15
      true;
    make_card_exists_test2
      "Testing to see what ids remain in the list 16" test_cc_json3 16
      true;
  ]

let card_in_map_tests =
  [
    make_card_in_map_test "Testing to see if card is in map 1"
      test_cc_json2 1
      {
        id = 1;
        description = "Advance to Go (Collect 200 BRB's)";
        action = "MoveToAndOwe";
        money_change = 200;
        move_to_pos = 0;
      };
    make_card_in_map_test "Testing to see if card is in map 1"
      test_cc_json2 2
      {
        id = 2;
        description = "Bank error in your favor. Collect $200";
        action = "MoneyChange";
        money_change = 200;
        move_to_pos = 39;
      };
    make_card_in_map_test "Testing to see if card is in map 1"
      test_cc_json3 2
      {
        id = 2;
        description = "Bank error in your favor. Collect $200";
        action = "MoneyChange";
        money_change = 200;
        move_to_pos = 39;
      };
  ]

let suite =
  "test suite for player.ml"
  >::: List.flatten
         [ cards_left_tests; card_exists_tests; card_in_map_tests ]

let _ = run_test_tt_main suite