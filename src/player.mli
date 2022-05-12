(* Interface for the player module detailing what the player should
   contain.*)

type t
(** The abstract type of values representing the player *)

type pos_id = int
(** The type of position ids *)

type name = string
(** The type of the player's name *)

type money = int
(** The type of the player's money *)

type property_id = pos_id
(** The type of the player's properties *)

val get_name : t -> name
(** [get_name t] retuns the name of a player [t]*)

val get_pos : t -> pos_id
(** [get_pos t] retuns the current position of a player [t]*)

val get_money : t -> money
(** [get_money t] retuns the current money of a player [t]*)

val get_properties : t -> property_id list
(** [get_properties t] retuns the list of properties of a player [t]*)

val init_player : name -> t
(** [init_player name] creates a player at the beginning of the game
    that has its player name be [name] *)

val move_forward : int -> t -> t
(** [move_forward num t] retuns a new player [t] with an updated
    position of the current position + [num]*)

val move_to_pos : pos_id -> t -> t
(** [move_to_pos t] retuns a new player [t] with an updated position of
    [id]*)

val change_money : int -> t -> t
(** [change_money amt t] returns a new player [t] with an updated amount
    of money which is the current players amount of money + [amt]*)

val add_property : property_id -> t -> t
(** [add_property property t] returns a new player [t] with an updated
    amount of properties which is the players current amount of
    properties in addition to [property]. Requires: [property] is not
    currently owned by the player*)

val add_properties : property_id list -> t -> t
(** [add_properties properties t] returns a new player [t] with an
    updated amount of properties which is the players current amount of
    properties in addition to [properties]. Requires: None of the
    elements in [properties] are currently owned by the player*)

val remove_property : property_id -> t -> t
(** [remove_property property t] returns a new player [t] with an
    updated amount of properties which is the players current amount of
    properties without [property]. Requires: [property] is currently
    owned by the player*)

val remove_properties : property_id list -> t -> t
(** [remove_properties properties t] returns a new player [t] with an
    updated amount of properties which is the players current amount of
    properties without the properties in [properties]. Requires: All of
    the elements in [properties] are currently owned by the player*)
