val p_buy_property : Game_board.t -> int -> Player.t -> Player.t
(*[p_buy_property player board prop_id] returns a new player.t with an
  updated list of properties and updated currency field. player is the
  starting state of the player. board is the game board. prop_id is the
  id that is purchased*)

val g_board_buy_property :
  Player.t -> Game_board.t -> int -> Game_board.t
(*[g_board_buy_property name board prop_id] returns a new game_board.t
  with an updated list of properties. name is the name of the purchasing
  player. board is the game board. prop_id is the id that is purchased*)

val p_buy_house_hotel :
  Player.t -> Game_board.t -> int -> int -> Player.t
(*[p_buy_house_hotel player board prop_id house_number] is the updated
  player.t value after paying for house_number houses for the property
  corresponding to prop_id in game_board.*)

val g_board_buy_house_hotel :
  Player.t -> Game_board.t -> int -> int -> Game_board.t
(*[g_board_buy_house_hotel player board prop_id house_number] is the
  updated game_board state after adding house_number houses to prop_id
  in game_board.t*)
