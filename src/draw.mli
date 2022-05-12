(** Interface for randomly selecting a community chest or chance card
    from the remaining cards in the deck *)

type t
(** The abstract type of the value representing the card that is drawn *)

type action =
  | MoveToSpace of int
  | MoveToBusStop
  | MoneyChange of int
  | MoveToAndOwe of int * int
  | GetOutOfJail
  | CollectFromPlayers of int
  | PayForHouseRepairs of int * int

type card = {
  description : string;
  action : action;
}

type cc_card = {
  id : int;
  description : string;
  action : string;
  money_change : int;
  move_to_pos : int;
}

exception NoCardsLeft of string

val from_json : Yojson.Basic.t -> t
(** [from_json json] is the starting set of cards that the game has to
    offer*)

val init_standard_deck : t
(**[init_standard_deck] is the starting deck of cards using the generic
   json*)

val draw_community_chest : t -> t * card
(**[draw_community_chest t] takes in the [t] cards that represent the
   two decks to draw from and draws from the community chest cards and
   returns a tuple containing the updated decks of cards as well as the
   card that we just drew. Raises a NoCardsLeft error if there are no
   remaining cards in the deck*)

val draw_chance : t -> t * card
(**[draw_chance t] takes in the [t] cards that represent the two decks
   to draw from and draws from the random cards and returns a tuple
   containing the updated decks of cards as well as the card that we
   just drew. Raises a NoCardsLeft error if there are no remaining cards
   in the deck*)

val cc_cards_left : t -> int
(** [cc_cards_left t] takes in a [t] that represent the two decks and
    returns the amount of cards that are left in the commnuity chest
    deck*)

val cc_card_exists : t -> int -> bool
(** [cc_card_exists t id] takes in an id and returns true if that id is
    in the ramaining deck of community chest cards and false if it isn't *)

val cc_card_inmap : t -> int -> cc_card
(** [cc_card_inmap t id] returns the the card that is associated with
    that id if there is one, or else it returns nothing*)
