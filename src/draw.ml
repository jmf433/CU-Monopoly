open Yojson.Basic.Util

exception NoCardsLeft of string

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

type random_card = {
  rid : int;
  rdescription : string;
  raction : string;
  rmoney_change : int;
  rmove_to_pos : int;
}

module CardMap = Map.Make (Int)

type ccMapType = cc_card CardMap.t

type randomMapType = random_card CardMap.t

type t = {
  community_chest : cc_card CardMap.t;
  random : random_card CardMap.t;
}

(* let to_action (str : string) (data : int * int) : action = match
   (str, data) with | "MoveToSpace", _ -> MoveToSpace | "MoveToBusStop",
   _ -> MoveToBusStop | "MoneyChange", (money, _) -> MoneyChange money |
   "MoveToAndOwe", (money, pos) -> MoveToAndOwe (money, pos) |
   "GetOutOfJail", _ -> GetOutOfJail | "CollectFromPlayers", (money, _)
   -> CollectFromPlayers money | "PayForHouseRepairs", (houses, hotels)
   -> PayForHouseRepairs (houses, hotels) | _ -> MoveToSpace *)

let from_ccjson json (acc : ccMapType) : cc_card CardMap.t =
  CardMap.add
    (json |> member "id" |> to_int)
    {
      id = json |> member "id" |> to_int;
      description = json |> member "description" |> to_string;
      action = json |> member "action" |> to_string;
      money_change = json |> member "money_change" |> to_int;
      move_to_pos = json |> member "move_to_pos" |> to_int;
    }
    acc

let from_randomjson json (acc : randomMapType) : random_card CardMap.t =
  CardMap.add
    (json |> member "id" |> to_int)
    {
      rid = json |> member "id" |> to_int;
      rdescription = json |> member "description" |> to_string;
      raction = json |> member "action" |> to_string;
      rmoney_change = json |> member "money_change" |> to_int;
      rmove_to_pos = json |> member "move_to_pos" |> to_int;
    }
    acc

let from_json json =
  let () = Unix.time () |> Float.to_int |> Random.init in
  {
    community_chest =
      List.fold_right from_ccjson
        (json |> member "community chest" |> to_list)
        CardMap.empty;
    random =
      List.fold_right from_randomjson
        (json |> member "random" |> to_list)
        CardMap.empty;
  }

let init_standard_deck =
  from_json (Yojson.Basic.from_file "data/default_cards.json")

let first_element = function
  | a, _, _ -> a

let second_element = function
  | _, b, _ -> b

let third_element = function
  | _, _, c -> c

let draw_cc_helper = function
  | { id; description; action; move_to_pos; _ }
    when action = "MoveToSpace" ->
      (id, description, MoveToSpace move_to_pos)
  | { id; description; action; _ } when action = "MoveToBusStop" ->
      (id, description, MoveToBusStop)
  | { id; description; action; money_change; _ }
    when action = "MoneyChange" ->
      (id, description, MoneyChange money_change)
  | { id; description; action; money_change; move_to_pos; _ }
    when action = "MoveToAndOwe" ->
      (id, description, MoveToAndOwe (move_to_pos, money_change))
  | { id; description; action; _ } when action = "GetOutOfJail" ->
      (id, description, GetOutOfJail)
  | { id; description; action; money_change; _ }
    when action = "CollectFromPlayers" ->
      (id, description, CollectFromPlayers money_change)
  | { id; description; action; _ } when action = "PayForHouseRepairs" ->
      (id, description, PayForHouseRepairs (40, 115))
  | _ -> failwith ""

let draw_community_chest (t : t) : t * card =
  if CardMap.is_empty t.community_chest then
    raise (NoCardsLeft "No CC Cards Remaining!")
  else
    let index = 17 - CardMap.cardinal t.community_chest in
    let card_check = CardMap.find index t.community_chest in
    let card_action = draw_cc_helper card_check in
    ( {
        community_chest =
          CardMap.remove (first_element card_action) t.community_chest;
        random = t.random;
      },
      {
        description = second_element card_action;
        action = third_element card_action;
      } )

let draw_cc_helper = function
  | { rid; rdescription; raction; rmove_to_pos; _ }
    when raction = "MoveToSpace" ->
      (rid, rdescription, MoveToSpace rmove_to_pos)
  | { rid; rdescription; raction; _ } when raction = "MoveToBusStop" ->
      (rid, rdescription, MoveToBusStop)
  | { rid; rdescription; raction; rmoney_change; _ }
    when raction = "MoneyChange" ->
      (rid, rdescription, MoneyChange rmoney_change)
  | { rid; rdescription; raction; rmoney_change; rmove_to_pos; _ }
    when raction = "MoveToAndOwe" ->
      (rid, rdescription, MoveToAndOwe (rmove_to_pos, rmoney_change))
  | { rid; rdescription; raction; _ } when raction = "GetOutOfJail" ->
      (rid, rdescription, GetOutOfJail)
  | { rid; rdescription; raction; rmoney_change; _ }
    when raction = "CollectFromPlayers" ->
      (rid, rdescription, CollectFromPlayers rmoney_change)
  | { rid; rdescription; raction; _ }
    when raction = "PayForHouseRepairs" ->
      (rid, rdescription, PayForHouseRepairs (25, 100))
  | _ -> failwith ""

let draw_chance (t : t) : t * card =
  if CardMap.is_empty t.random then
    raise (NoCardsLeft "No Chance Cards Remaining!")
  else
    let index = 14 - CardMap.cardinal t.random in
    let card_check = CardMap.find index t.random in
    let card_action = draw_cc_helper card_check in
    ( {
        community_chest =
          CardMap.remove (first_element card_action) t.community_chest;
        random = t.random;
      },
      {
        description = second_element card_action;
        action = third_element card_action;
      } )

let cc_cards_left t = CardMap.cardinal t.community_chest

let cc_card_exists t (id : int) = CardMap.mem id t.community_chest

let cc_card_inmap t id = CardMap.find id t.community_chest
