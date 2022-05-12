open OUnit2
open Final_project
open Game_board

let board = from_json (Yojson.Basic.from_file "data/game_board.json")

let brd_1_purchase = buy_property board 1 "Jacob"

let brd_rr_1_purchase = buy_property board 4 "Jacob"

let brd_rr_2_purchase = buy_property brd_rr_1_purchase 5 "Jacob"

let brd_rr_3_purchase = buy_property brd_rr_2_purchase 6 "Jacob"

let get_rent_tests =
  [
    ( "Standard rent test, no houses" >:: fun _ ->
      assert_equal (Rent 100) (landing_action brd_1_purchase 1) );
    ( "No owner rent lookup" >:: fun _ ->
      assert_equal Unowned (landing_action board 1) );
    ( "Unownable property error" >:: fun _ ->
      assert_equal Invalid (landing_action board 0) );
  ]

let purchase_price_tests =
  [
    ( "Standard price test" >:: fun _ ->
      assert_equal 100 (purchase_price board 1) );
    ( "Invalid ID error" >:: fun _ ->
      assert_raises (InvalidProperty 0) (fun () ->
          purchase_price board 0) );
  ]

let house_price_tests =
  [
    ( "Standard house price test" >:: fun _ ->
      assert_equal 50 (house_price board 1) );
    ( "Invalid ID error" >:: fun _ ->
      assert_raises (InvalidProperty 0) (fun () -> house_price board 0)
    );
  ]

let buy_houses_tests =
  [
    ( "Buying first house test" >:: fun _ ->
      assert_equal (Rent 200)
        (landing_action (buy_houses brd_1_purchase 1 1) 1) );
    ( "Buying two houses test" >:: fun _ ->
      assert_equal (Rent 300)
        (landing_action (buy_houses brd_1_purchase 1 2) 1) );
    ( "Buying too many houses test" >:: fun _ ->
      assert_equal (Rent 300)
        (landing_action (buy_houses brd_1_purchase 1 6) 1) );
    ( "Invalid ID error" >:: fun _ ->
      assert_raises (InvalidProperty 0) (fun () -> buy_houses board 0 5)
    );
  ]

let buy_property_tests =
  [
    ( "Buying property test" >:: fun _ ->
      assert_equal "Jacob" (owner brd_1_purchase 1) );
  ]

let printer = function
  | Some n -> string_of_int n
  | None -> "None"

let railroad_price_tests =
  [
    ( "One railroad test" >:: fun _ ->
      assert_equal (Rent 100) (landing_action brd_rr_1_purchase 4) );
    ( "Two railroad test 1" >:: fun _ ->
      assert_equal (Rent 200) (landing_action brd_rr_2_purchase 4) );
    ( "Two railroad test 2" >:: fun _ ->
      assert_equal (Rent 201) (landing_action brd_rr_2_purchase 5) );
    ( "Three railroad test 1" >:: fun _ ->
      assert_equal (Rent 300) (landing_action brd_rr_3_purchase 4) );
    ( "Three railroad test 2" >:: fun _ ->
      assert_equal (Rent 301) (landing_action brd_rr_3_purchase 5) );
    ( "Three railroad test 3" >:: fun _ ->
      assert_equal (Rent 302) (landing_action brd_rr_3_purchase 6) );
  ]

let suite =
  "test suite for player.ml"
  >::: List.flatten
         [
           purchase_price_tests;
           get_rent_tests;
           buy_houses_tests;
           house_price_tests;
           buy_property_tests;
           railroad_price_tests;
         ]

let _ = run_test_tt_main suite