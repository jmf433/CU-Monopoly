open OUnit2
open Final_project
open Player

let make_get_name_test
    (name : string)
    (t : t)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (get_name t) ~printer:Fun.id

let make_get_pos_test (name : string) (t : t) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (get_pos t) ~printer:string_of_int

let make_get_money_test (name : string) (t : t) (expected_output : int)
    =
  name >:: fun _ ->
  assert_equal expected_output (get_money t) ~printer:string_of_int

let make_get_properties_test
    (name : string)
    (t : t)
    (expected_output : int list) =
  name >:: fun _ -> assert_equal expected_output (get_properties t)

let basic_player = Player.init_player "John Doe"

let get_name_tests =
  [
    make_get_name_test "Testing the inital name John Doe" basic_player
      "John Doe";
    make_get_name_test "Testing the inital name OCaml"
      (init_player "OCaml") "OCaml";
  ]

let get_pos_tests =
  [ make_get_pos_test "Testing the inital position" basic_player 0 ]

let get_money_tests =
  [
    make_get_money_test "Testing the init amount of money" basic_player
      1000;
  ]

let get_properties_tests =
  [
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      basic_player [];
  ]

let basic_player2 = move_forward 10 basic_player

let basic_player3 = move_forward 12 basic_player2

let basic_player4 = move_forward 12 basic_player3

let basic_player5 = move_forward 10 basic_player4

let basic_player6 = move_forward 6 basic_player4

let basic_player7 = move_forward 5 basic_player4

let move_forward_tests =
  [
    make_get_pos_test "Testing the player moving 10 spaces from start"
      basic_player2 10;
    make_get_pos_test "Testing the player moving from 10 to 22"
      basic_player3 22;
    make_get_pos_test "Testing the player moving from 22 to 34"
      basic_player4 34;
    make_get_pos_test "Testing the player moving from 34 to 4"
      basic_player5 4;
    make_get_pos_test "Testing the player moving from 34 to 0"
      basic_player6 0;
    make_get_pos_test "Testing the player moving from 34 to 39"
      basic_player7 39;
  ]

let move_to_pos_tests =
  [
    make_get_pos_test "Testing the player moving from 0 to 25"
      (move_to_pos 25 basic_player)
      25;
    make_get_pos_test "Testing the player moving from 39 to 25"
      (move_to_pos 25 basic_player6)
      25;
    make_get_pos_test
      "Testing the player moving from 39 to 39 which means it is \
       unchanged"
      (move_to_pos 39 basic_player7)
      39;
  ]

let change_money_tests =
  [
    make_get_money_test "Testing the init amount of money"
      (change_money 30 basic_player)
      1030;
    make_get_money_test "Testing the init amount of money"
      (change_money (-1000) basic_player)
      0;
    make_get_money_test "Testing the init amount of money"
      (change_money 1000 basic_player)
      2000;
    make_get_money_test "Testing the init amount of money"
      (change_money 0 basic_player)
      1000;
  ]

let change_properties_tests =
  [
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      (add_property 2 basic_player)
      [ 2 ];
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      (add_property 2 basic_player |> add_property 1)
      [ 1; 2 ];
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      (add_property 2 basic_player |> add_property 1 |> add_property 3)
      [ 3; 1; 2 ];
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      (add_property 2 basic_player |> remove_property 2)
      [];
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      (add_property 2 basic_player
      |> add_property 3 |> remove_property 2)
      [ 3 ];
    make_get_properties_test
      "Testing the init properties that are owned by a player"
      (add_property 2 basic_player
      |> add_property 3 |> remove_property 2 |> remove_property 3)
      [];
  ]

let suite =
  "test suite for player.ml"
  >::: List.flatten
         [
           get_name_tests;
           get_pos_tests;
           get_money_tests;
           get_properties_tests;
           move_forward_tests;
           move_to_pos_tests;
           change_money_tests;
           change_properties_tests;
         ]

let _ = run_test_tt_main suite