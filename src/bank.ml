let enough_money (player : Player.t) (money : int) : bool =
  Player.get_money player >= money

let p_buy_property
    (board : Game_board.t)
    (prop_id : int)
    (player : Player.t) : Player.t =
  if enough_money player (Game_board.purchase_price board prop_id) then
    Player.add_property prop_id
      (Player.change_money
         (-1 * Game_board.purchase_price board prop_id)
         player)
  else player

let g_board_buy_property
    (player : Player.t)
    (board : Game_board.t)
    (prop_id : int) : Game_board.t =
  if enough_money player (Game_board.purchase_price board prop_id) then
    Game_board.buy_property board prop_id (Player.get_name player)
  else board

let p_buy_house_hotel
    (player : Player.t)
    (board : Game_board.t)
    (prop_id : int)
    (house_number : int) : Player.t =
  if
    enough_money player
      (house_number * Game_board.house_price board prop_id)
    && Game_board.num_house board prop_id + house_number <= 4
  then
    Player.change_money
      (-1 * house_number * Game_board.house_price board prop_id)
      player
  else player

let g_board_buy_house_hotel
    (player : Player.t)
    (board : Game_board.t)
    (prop_id : int)
    (house_number : int) : Game_board.t =
  if
    enough_money player
      (house_number * Game_board.house_price board prop_id)
    && Game_board.num_house board prop_id + house_number <= 4
  then Game_board.buy_houses board prop_id house_number
  else board
