open Final_project.Game_board
open Final_project.Player
open Final_project.Bank
open Final_project.Draw

type game_board = Final_project.Game_board.t

type player = Final_project.Player.t

type deck = Final_project.Draw.t

(**Ends the game, with [name] as the winner*)
let game_over (name : string) :
    (int * player list * game_board * 'a) * bool =
  print_endline ("\n" ^ name ^ " has won the game!");
  exit 0

(**Updates the player via f corresponding to index in a list of players.*)
let update_one_player
    (index : int)
    (players : player list)
    (f : player -> player) =
  List.mapi
    (fun ind player -> if ind = index then f player else player)
    players

let rec find_player (name : string) (players : player list) =
  match players with
  | [] -> raise (Failure "Player not found")
  | h :: t ->
      if name = get_name h then (h, 0)
      else
        let result, acc = find_player name t in
        (result, acc + 1)

let remove_one_player (index : int) (players : player list) =
  List.filteri (fun i _ -> index != i) players

(**Prints how much money the player n has*)
let print_money player =
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    (get_name player ^ " has "
    ^ string_of_int (get_money player)
    ^ " BRBs. \n")

let print_center styles str : unit =
  let x, y = ANSITerminal.size () in
  let x = (x / 2) - (String.length str / 2) in
  ANSITerminal.move_cursor x y;
  ANSITerminal.print_string styles str;
  print_string "\n";
  ANSITerminal.move_bol ()

(**Prints the correct text at the start of a turn*)
let pre_move_text
    (turn : int)
    (players : player list)
    (board_state : Final_project.Game_board.t) =
  let curr_name = get_name (List.nth players turn) in
  let curr_pos = get_pos (List.nth players turn) in
  let x, _ = ANSITerminal.size () in
  print_string (String.make x '-');
  print_string "\n";
  print_center [ ANSITerminal.green ]
    ("It is " ^ curr_name ^ "'s turn \n");
  print_endline
    (curr_name ^ " is currently at "
    ^ get_prop_name board_state curr_pos);
  print_money (List.nth players turn);
  print_endline "Press enter to roll dice for your turn:"

(**Handles the dice roll for a player's turn*)
let dice_roll_text (turn : int) (players : player list) =
  let curr_name = get_name (List.nth players turn) in
  match read_line () with
  | _ ->
      let r1, r2 = Final_project.Dice_roll.two_rolls 6 in
      print_endline (curr_name ^ " has rolled a ");
      Final_project.Dice_roll.pretty_print_dice (r1, r2);
      let new_players =
        update_one_player turn players (move_forward (r1 + r2))
      in
      (new_players, r1 + r2)

(**Gives the opportunity to purchase a property*)
let rec purchase_opportunity input =
  match input with
  | "y"
  | "yes"
  | "Yes" ->
      true
  | "n"
  | "no"
  | "No" ->
      false
  | _ ->
      print_string "> ";
      purchase_opportunity (read_line ())

let rec print_player_list turn players acc =
  match players with
  | [] -> ()
  | h :: t ->
      if acc != turn then
        print_endline (string_of_int (acc + 1) ^ ": " ^ get_name h);
      print_player_list turn t (acc + 1)

let rec print_properties board_state properties =
  match properties with
  | [] -> print_string "\n"
  | h :: t ->
      let suff =
        if mortgage_status board_state h then " - Currently mortgaged"
        else ""
      in
      let price =
        " - worth "
        ^ string_of_int (purchase_price board_state h)
        ^ " BRBs"
      in
      let houses =
        " - " ^ string_of_int (num_house board_state h) ^ " Houses"
      in
      ANSITerminal.print_string
        [ ANSITerminal.magenta ]
        ("id " ^ string_of_int h ^ ": "
        ^ get_prop_name board_state h
        ^ houses ^ price ^ suff);
      print_properties board_state t

let print_player_possessions player board_state =
  print_string "\n";
  print_money player;
  if List.length (get_properties player) > 0 then
    print_endline
      ("In addition, " ^ get_name player
     ^ " owns the following properties:");
  print_properties board_state (get_properties player)

let print_trade_status
    player1
    player2
    board_state
    (offer_money, offer_list)
    (receive_money, receive_list) =
  print_endline (get_name player1 ^ " will give: ");
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    (string_of_int offer_money ^ " BRBs \n");
  print_properties board_state offer_list;
  print_endline (get_name player2 ^ " will give: ");
  ANSITerminal.print_string
    [ ANSITerminal.magenta ]
    (string_of_int receive_money ^ " BRBs \n");
  print_properties board_state receive_list

let finalize_trade
    ind1
    ind2
    players
    board_state
    (offer_money, offer_list)
    (receive_money, receive_list) : player list * game_board =
  let player1 = List.nth players ind1 in
  let player2 = List.nth players ind2 in
  let board_state =
    remove_player board_state offer_list (get_name player2)
  in
  let board_state =
    remove_player board_state receive_list (get_name player1)
  in
  let player1 = add_properties receive_list player1 in
  let player1 = remove_properties offer_list player1 in
  let player2 = add_properties offer_list player2 in
  let player2 = remove_properties receive_list player2 in

  let net_money = receive_money - offer_money in
  let player1 = change_money net_money player1 in
  let player2 = change_money (-net_money) player2 in
  let players = update_one_player ind1 players (fun _ -> player1) in
  let players = update_one_player ind2 players (fun _ -> player2) in
  print_center [ ANSITerminal.green ] "The trade has been completed! \n";
  (players, board_state)

let confirm_trade
    ind1
    ind2
    players
    board_state
    (offer_money, offer_list)
    (receive_money, receive_list) =
  print_trade_status (List.nth players ind1) (List.nth players ind2)
    board_state
    (offer_money, offer_list)
    (receive_money, receive_list);
  print_center [ ANSITerminal.red ]
    (get_name (List.nth players ind2)
    ^ ": please type yes to confirm this trade, or no to deny \n");
  print_string "> ";
  if purchase_opportunity (read_line ()) then
    finalize_trade ind1 ind2 players board_state
      (offer_money, offer_list)
      (receive_money, receive_list)
  else (players, board_state)

let rec trade_loop
    ind1
    ind2
    players
    props1
    props2
    board_state
    (offer_money, offer_list)
    (receive_money, receive_list) : player list * game_board =
  print_string "> ";
  let input = read_line () in
  let offer, receive, keep_trading =
    if input = "" then
      ((offer_money, offer_list), (receive_money, receive_list), false)
    else if input = "status" || input = "Status" then (
      print_trade_status (List.nth players ind1) (List.nth players ind2)
        board_state
        (offer_money, offer_list)
        (receive_money, receive_list);
      print_string "\n";
      ((offer_money, offer_list), (receive_money, receive_list), true))
    else
      match int_of_string_opt input with
      (* | "" -> confirm_trade ind1 ind2 players board_state offer_list
         receive_list *)
      | Some n when List.mem n props1 && not (List.mem n offer_list) ->
          print_endline
            ("You have added "
            ^ get_prop_name board_state n
            ^ " to your offer.");
          ( (offer_money, n :: offer_list),
            (receive_money, receive_list),
            true )
      | Some n when List.mem n props1 && List.mem n offer_list ->
          print_endline
            ("You have removed "
            ^ get_prop_name board_state n
            ^ " from your offer.");
          ( (offer_money, List.filter (fun v -> v != n) offer_list),
            (receive_money, receive_list),
            true )
      | Some n when List.mem n props2 && not (List.mem n receive_list)
        ->
          print_endline
            ("You have added "
            ^ get_prop_name board_state n
            ^ " to your ask.");
          ( (offer_money, offer_list),
            (receive_money, n :: receive_list),
            true )
      | Some n when List.mem n props2 && List.mem n receive_list ->
          print_endline
            ("You have removed "
            ^ get_prop_name board_state n
            ^ " from your ask.");
          ( (offer_money, offer_list),
            (receive_money, List.filter (fun v -> v != n) receive_list),
            true )
      | Some _ ->
          print_endline "Invalid entry.";
          ( (offer_money, offer_list),
            (receive_money, receive_list),
            true )
      | None ->
          if input.[0] = '$' then (
            match
              int_of_string_opt
                (String.sub input 1 (String.length input - 1))
            with
            | Some n when n != 0 ->
                if n > 0 && n <= get_money (List.nth players ind1) then (
                  print_endline
                    ("You have offered " ^ string_of_int n ^ " BRBs.");
                  ((n, offer_list), (0, receive_list), true))
                else if n < 0 && -n <= get_money (List.nth players ind2)
                then (
                  print_endline
                    ("You have requested " ^ string_of_int (-n)
                   ^ " BRBs");
                  ((0, offer_list), (-n, receive_list), true))
                else (
                  (*n is too much money*)
                  print_endline "Requested / Offered too much money";
                  ( (offer_money, offer_list),
                    (receive_money, receive_list),
                    true ))
            | Some _ ->
                (*n is 0*)
                print_endline
                  "You have removed all money from the offer.";
                ((0, offer_list), (0, receive_list), true)
            | None ->
                print_endline "Invalid entry.";
                ( (offer_money, offer_list),
                  (receive_money, receive_list),
                  true ))
          else (
            print_endline "Invalid entry.";
            ( (offer_money, offer_list),
              (receive_money, receive_list),
              true ))
  in
  if keep_trading then
    trade_loop ind1 ind2 players props1 props2 board_state offer receive
  else confirm_trade ind1 ind2 players board_state offer receive

let init_trade ind1 ind2 players board_state : player list * game_board
    =
  let player1 = List.nth players ind1 in
  let player2 = List.nth players ind2 in

  print_player_possessions player1 board_state;
  print_string "\n";
  print_player_possessions player2 board_state;
  print_string "\n";

  print_center [] "INSTRUCTIONS:";

  print_endline
    "Type a property id to add it or remove it from the trade.";
  print_endline
    "Type \"$\" with a positive number to add your own money to the \
     trade.";
  print_endline
    ("Type \"$\" with a negative number to add " ^ get_name player2
   ^ "'s money to the trade. \n ");
  print_endline "Type $0 to remove all money from the trade. \n ";
  print_endline
    "Type status to look at the items currently being traded";
  trade_loop ind1 ind2 players
    (get_properties player1)
    (get_properties player2)
    board_state (0, []) (0, [])

let rec trade turn players board_state : player list * game_board =
  print_endline "Please enter a player id number to trade with:";
  print_player_list turn players 0;
  print_string "> ";
  let players, board_state =
    match int_of_string_opt (read_line ()) with
    | Some n when n <= List.length players && n != turn + 1 ->
        init_trade turn (n - 1) players board_state
    | _ -> trade turn players board_state
  in
  (players, board_state)

(**Handles the opportunity to purchase a property*)
let purchase_opportunity_text
    curr_name
    curr_pos
    curr_prop
    turn
    players
    board_state
    deck_state =
  print_endline "This property is available!";
  if
    get_money (List.nth players turn)
    >= purchase_price board_state curr_pos
  then (
    print_center []
      ("Would " ^ curr_name ^ " like to purchase it for "
      ^ string_of_int (purchase_price board_state curr_pos)
      ^ " BRBs? (Please input yes/no)");
    print_string "> ";
    if purchase_opportunity (read_line ()) then (
      print_center [ ANSITerminal.green ]
        (curr_name ^ " has purchased " ^ curr_prop ^ "!");
      print_string "\n";
      let updated_players =
        update_one_player turn players
          (p_buy_property board_state curr_pos)
      in
      let updated_board_state =
        buy_property board_state curr_pos curr_name
      in
      (turn, updated_players, updated_board_state, deck_state))
    else (turn, players, board_state, deck_state))
  else (
    ANSITerminal.print_string [ ANSITerminal.red ]
      "However, you do not have enough money to purchase.";
    (turn, players, board_state, deck_state))

let rec sell_mortgage_text
    (turn : int)
    (players : player list)
    board_state
    deck_state =
  let unmortgaged_props =
    List.filter
      (fun n -> not (mortgage_status board_state n))
      (get_properties (List.nth players turn))
  in
  if List.length unmortgaged_props > 0 then (
    print_endline
      "Type any number listed below to mortgage the corresponding \
       property, or to sell one house if any exist. Just press enter \
       to exit this menu.";
    print_properties board_state unmortgaged_props;
    sell_mortgage_choice turn players board_state deck_state)
  else (
    print_endline "No properties to mortgage.";
    (turn, players, board_state, deck_state))

and sell_mortgage_choice turn players board_state deck_state =
  print_string "> ";
  let prop_list = get_properties (List.nth players turn) in
  let input = read_line () in
  if input = "" then (turn, players, board_state, deck_state)
  else
    match int_of_string_opt input with
    | None ->
        print_endline "Invalid choice.";
        sell_mortgage_choice turn players board_state deck_state
    | Some n
      when List.mem n prop_list && not (mortgage_status board_state n)
      ->
        if num_house board_state n > 0 then (
          (*Selling one house*)
          print_endline
            ("You have sold 1 house from " ^ get_prop_name board_state n);
          let updated_players =
            update_one_player turn players
              (change_money (house_price board_state n / 2))
          in
          let updated_board = buy_houses board_state n (-1) in
          print_money (List.nth updated_players turn);
          sell_mortgage_choice turn updated_players updated_board
            deck_state)
        else (
          print_endline
            ("You have mortgaged "
            ^ get_prop_name board_state n
            ^ " for "
            ^ string_of_int (purchase_price board_state n / 2)
            ^ " BRBs");
          let updated_players =
            update_one_player turn players
              (change_money (purchase_price board_state n / 2))
          in
          let updated_board = change_mortgage board_state n true in
          print_money (List.nth updated_players turn);
          sell_mortgage_choice turn updated_players updated_board
            deck_state)
    | _ ->
        print_endline "Invalid choice.";
        sell_mortgage_choice turn players board_state deck_state

let rec action_choice turn players board_state deck_state =
  print_center []
    "Type one of the following options to perform it, or enter to end \
     your turn.";
  print_endline "1: Trade";
  print_endline "2: Mortgage";
  print_string "> ";
  match read_line () with
  | "1"
  | "Trade"
  | "trade" ->
      let players, board_state = trade turn players board_state in
      action_choice turn players board_state deck_state
  | "2"
  | "Mortgage"
  | "mortgage" ->
      let _, players, board_state, _ =
        sell_mortgage_text turn players board_state deck_state
      in
      action_choice turn players board_state deck_state
  | "" -> (players, board_state)
  | _ -> action_choice turn players board_state deck_state

let pay_tax price turn players board_state deck_state =
  (*This will crash if the player goes bankrupt due to taxes*)
  let updated_players =
    update_one_player turn players (change_money (-1 * price))
  in
  print_money (List.nth players turn);
  (turn, updated_players, board_state, deck_state)

(**Handles the payment of rent*)
let rec pay_rent_text
    curr_name
    curr_pos
    curr_prop
    rent
    turn
    players
    board_state
    deck_state =
  print_endline (owner board_state curr_pos ^ " owns " ^ curr_prop);
  print_endline ("The rent is " ^ string_of_int rent ^ " BRBs");
  let turn, updated_players, board_state, deck_state =
    let owner, owner_index =
      find_player (owner board_state curr_pos) players
    in
    if get_money (List.nth players turn) >= rent then (
      (*If enough money*)
      let updated_players =
        update_one_player turn players (change_money (-1 * rent))
      in
      let updated_players =
        update_one_player owner_index updated_players
          (change_money rent)
      in
      print_money (List.nth updated_players turn);
      print_money (List.nth updated_players owner_index);
      (turn, updated_players, board_state, deck_state))
    else
      let (turn, players, board_state, deck_state), still_pay =
        if
          List.length
            (List.filter
               (fun n -> not (mortgage_status board_state n))
               (get_properties (List.nth players turn)))
          > 0
        then (
          print_endline
            (curr_name
           ^ " does not have enough money to pay this rent, and must \
              mortgage properties");
          (sell_mortgage_text turn players board_state deck_state, true))
        else
          let updated_players =
            update_one_player owner_index players
              (add_properties (get_properties (List.nth players turn)))
          in
          let updated_board =
            remove_player board_state
              (get_properties (List.nth players turn))
              (get_name owner)
          in
          let updated_players =
            remove_one_player turn updated_players
          in
          print_endline
            (curr_name
           ^ " has no properties to mortgage, and is therefore out of \
              the game. All of their properties have been reassigned \
              to " ^ get_name owner);
          if List.length updated_players > 1 then
            ( (turn - 1, updated_players, updated_board, deck_state),
              false )
          else game_over (get_name owner)
      in
      if still_pay then
        pay_rent_text curr_name curr_pos curr_prop rent turn players
          board_state deck_state
      else (turn, players, board_state, deck_state)
  in
  (turn, updated_players, board_state, deck_state)

(**[count_house_hotel] is the tuple (house, hotel), where house is the
   number of houses owned, and hotel is the number of hotels owned*)
let count_house_hotel player board_state : int * int =
  let owned_properties = get_properties player in
  List.fold_left
    (fun (house, hotel) prop ->
      if num_house board_state prop = 4 then (house, hotel + 1)
      else (house + num_house board_state prop, hotel))
    (0, 0) owned_properties

let draw_card_text
    turn
    players
    board_state
    deck_state
    (f :
      Final_project.Draw.t ->
      Final_project.Draw.t * Final_project.Draw.card) =
  let new_deck_state, ({ description; action } : card) = f deck_state in
  print_endline description;
  match action with
  | MoveToSpace p ->
      let updated_players =
        update_one_player turn players (move_to_pos p)
      in
      (turn, updated_players, board_state, new_deck_state)
  | MoveToBusStop ->
      let updated_players =
        update_one_player turn players
          (move_to_pos
             (((get_pos (List.nth players turn) + 5) / 10 * 10) + 5))
      in
      (turn, updated_players, board_state, new_deck_state)
  | MoneyChange n ->
      let updated_players =
        update_one_player turn players (change_money n)
      in
      (turn, updated_players, board_state, new_deck_state)
  | MoveToAndOwe (p, n) ->
      let updated_players =
        update_one_player turn players (move_to_pos p)
      in
      let updated_players =
        update_one_player turn updated_players (change_money n)
      in
      print_money (List.nth players turn);
      (turn, updated_players, board_state, new_deck_state)
  | GetOutOfJail -> failwith "test"
  | CollectFromPlayers n ->
      let updated_players = List.map (change_money (-1 * n)) players in
      let updated_players =
        update_one_player turn updated_players
          (change_money (n * List.length players))
      in
      (turn, updated_players, board_state, new_deck_state)
  | PayForHouseRepairs (house_price, hotel_price) ->
      let houses, hotels =
        count_house_hotel (List.nth players turn) board_state
      in
      let cost =
        (-1 * (houses * house_price)) + (hotels * hotel_price)
      in
      let updated_players =
        update_one_player turn players (change_money cost)
      in
      (turn, updated_players, board_state, new_deck_state)

(**Handles the interface up until the dice roll*)
let rec pre_move_game_loop
    (turn : int)
    (players : player list)
    (board_state : Final_project.Game_board.t)
    (deck_state : Final_project.Draw.t) =
  pre_move_text turn players board_state;
  let new_players, roll = dice_roll_text turn players in
  post_move_game_loop roll turn new_players board_state deck_state

(**Handles the interface post dice roll*)
and post_move_game_loop
    (roll : int)
    (turn : int)
    (players : player list)
    (board_state : Final_project.Game_board.t)
    (deck_state : Final_project.Draw.t) =
  let curr_player = List.nth players turn in
  let curr_name = get_name curr_player in
  let curr_pos = get_pos curr_player in
  let curr_prop = get_prop_name board_state curr_pos in
  print_center [] (curr_name ^ " is now at " ^ curr_prop);
  print_center []
    ("Description: " ^ get_prop_description board_state curr_pos);
  let turn, players, board_state, deck_state =
    match
      landing_action board_state (get_pos (List.nth players turn))
    with
    | Unowned ->
        purchase_opportunity_text curr_name curr_pos curr_prop turn
          players board_state deck_state
    | Rent rent ->
        if not (List.mem curr_pos (get_properties curr_player)) then
          pay_rent_text curr_name curr_pos curr_prop rent turn players
            board_state deck_state
        else (
          print_endline (curr_name ^ " owns this property!");
          (turn, players, board_state, deck_state))
    | Mortgaged ->
        print_endline "This property is currently mortgaged.";
        (turn, players, board_state, deck_state)
    | DrawCard s ->
        print_endline ("You have drawn a " ^ s ^ " card!");
        if s = "Chance" then
          draw_card_text turn players board_state deck_state draw_chance
        else
          draw_card_text turn players board_state deck_state
            draw_community_chest
    | Tax n -> pay_tax n turn players board_state deck_state
    | UtilityRent n ->
        pay_rent_text curr_name curr_pos curr_prop (roll * n) turn
          players board_state deck_state
    | GoJail ->
        ( turn,
          update_one_player turn players (move_to_pos 10),
          board_state,
          deck_state )
    | Invalid -> (turn, players, board_state, deck_state)
  in
  let players, board_state =
    action_choice turn players board_state deck_state
  in
  pre_move_game_loop
    ((turn + 1) mod List.length players)
    players board_state deck_state

(**Handles entry of names*)
let rec name_entry acc =
  let pref, inf =
    match List.length acc with
    | 0 -> ("Please e", "1st")
    | 1 -> ("Please e", "2nd")
    | 2 -> ("Press 'Enter' to play with 2 players, or e", "3rd")
    | 3 -> ("Press 'Enter' to play with 3 players, or e", "4th")
    | _ -> failwith "Too Many Players"
  in
  print_endline (pref ^ "nter the " ^ inf ^ " player's name");
  print_string "> ";
  match read_line () with
  | "" when List.length acc >= 2 ->
      print_endline "The game will now begin!";
      acc
  | "" -> name_entry acc
  | name when List.length acc = 3 -> init_player name :: acc
  | name -> name_entry (init_player name :: acc)

let main () =
  ANSITerminal.print_string [] "\nWelcome to Cornell Monopoly (MS2)! \n";
  let name_list = List.rev (name_entry []) in
  pre_move_game_loop 0 name_list init_standard_board init_standard_deck

let () = main ()