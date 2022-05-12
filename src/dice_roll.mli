(**Representation of a die in the monopoly game.

   This module contains the functions of the die and special
   interactions *)

(* val first_dice_roll : roll -> roll (** [first_dice_roll roll] returns
   the first roll of two die with a max roll of [roll] Example:
   first_dice_roll 6 -> 4*)

   val second_dice_roll : roll -> roll (** [second_dice_roll roll]
   returns the second roll of two die with a max roll of [roll]

   Example: second_dice_roll 6 -> 3*) *)

type roll = int
(** The type of the max roll of die *)

val two_rolls : roll -> roll * roll
(**[two_rolls max] returns a tuple of two random rolls of die with a max
   roll of [max]-1

   Example two_rolls 7 -> (1, 6) *)

val total_roll : roll * roll -> roll
(** [total_roll two_rolls] returns the total roll of a tuple of two
    rolls [two_rolls]

    Example: total_roll (two_rolls 7) -> 3*)

val is_snake_eyes : roll * roll -> bool
(**[is_snake_eyes two_rolls] returns a boolean value on true if the
   first die roll and the second die role are the same roll value of 1
   else false

   Example: is_snake_eyes (1, 1) -> true, is_snake_eyes (1,2) -> false *)

val pretty_print_dice : roll * roll -> unit
(**[pretty_print_dice two_rolls] prints a pretty graphical
   representation of the dice roll.*)
