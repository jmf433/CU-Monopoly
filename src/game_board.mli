(**Representation of a game board state.

   This module contains all the information present on a game board. It
   handles the loading of game boards and the interactions that apply to
   a board*)

type t
(*A type representing a game board*)

type land_action =
  | Mortgaged (*Mortgaged Property*)
  | Rent of int (*Rent to pay*)
  | Unowned (*Unowned Standard Property*)
  | DrawCard of string (*Time to draw a card*)
  | Tax of int
  | UtilityRent of int
  | GoJail
  | Invalid

exception InvalidProperty of int

val from_json : Yojson.Basic.t -> t
(**[from_json json] is the default game board associated with the json *)

val init_standard_board : t
(**[init_standard_board] is the board initiated with the standard
   game_board json*)

val get_prop_name : t -> int -> string
(**[get_name board id] is the name of the property at id*)

val get_prop_description : t -> int -> string
(**[get_prop_description board id] is the description of the property at
   id*)

val owner : t -> int -> string
(**[owner board id] is the owner of the property at id. Raises Unowned
   if the property is unowned. Raises InvalidProperty if the property
   can't have an owner.*)

val purchase_price : t -> int -> int
(*[purchase_price board id] is the price to purchase the property at id
  in board t. Riases [InvalidProperty id] if the property cannot be
  purchased.*)

val landing_action : t -> int -> land_action
(*[lookup rent board id] is the action that occurs when someone lands on
  the property at id.*)

val house_price : t -> int -> int
(*[house_price board id] is the price to purchase one house for the
  property at id. Raises InvalidProperty if the property does not
  support houses*)

val num_house : t -> int -> int
(*[num_houses board id] is the number of houses on id in board. Raises 0
  if the id does not support houses.*)

val buy_houses : t -> int -> int -> t
(*[buy_houses board id houses] is the updated state after purchasing
  (houses) houses for the property at id. Raises [InvalidProperty id] if
  the property does not support houses*)

val buy_property : t -> int -> string -> t
(*[buy_property board id name] is the updated state after the player
  with name [name] purchases the property at [id]. Raises
  [InvalidProperty id] if the property cannot be purchased. Requires:
  [name] is a valid player name *)

val change_mortgage : t -> int -> bool -> t
(*[change_mortgage board id m] is the updated state after changing the
  mortgage status of the property at id to m. Raises [InvalidProperty
  id] if the property cannot be mortgaged*)

val mortgage_status : t -> int -> bool
(*[mortgage_status board id] is true if the property is mortgaged, and
  false if the property is not. Raises [InvalidProperty id] if the
  property cannot be mortgaged*)

val remove_player : t -> int list -> string -> t
(*[remove_player board properties new_owner]* is the updated board state
  after reassigning all properties in properties to new_owner*)
