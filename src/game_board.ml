open Yojson.Basic.Util

exception InvalidProperty of int

exception InvalidJson

exception Unowned

module PropertyMap = Map.Make (Int)

type group =
  | Central
  | South
  | North
  | West
  | East
  | Collegetown
  | NewNorth

type owner_options =
  | Player of string
  | None

type railroad_details = {
  owner : owner_options;
  rent_list : int list;
  price : int;
  mortgaged : bool;
}

type normal_details = {
  owner : owner_options;
  rent_list : int list;
  houses : int;
  price : int;
  house_price : int;
  region : group;
  mortgaged : bool;
}

type utility_details = {
  owner : owner_options;
  rent_list : int list;
  price : int;
  mortgaged : bool;
}

type property_option =
  | Normal of normal_details
  | Railroad of railroad_details
  | Draw of string
  | Tax of int
  | Jail
  | GoJail
  | Utility of utility_details
  | Go
  | Free

type property = {
  name : string;
  description : string;
  property_type : property_option;
}

type pMapType = property PropertyMap.t

type t = {
  properties : property PropertyMap.t;
  rr_locs : int list;
  util_locs : int list;
}

type land_action =
  | Mortgaged
  | Rent of int
  | Unowned
  | DrawCard of string
  | Tax of int
  | UtilityRent of int
  | GoJail
  | Invalid

let sort_group json : group =
  match json |> to_string with
  | "Central" -> Central
  | "South" -> South
  | "North" -> North
  | "West" -> West
  | "East" -> East
  | "Collegetown" -> Collegetown
  | "New North" -> NewNorth
  | s -> raise (Invalid_argument s)

let create_normal_property (type_data : Yojson.Basic.t) :
    property_option =
  Normal
    {
      owner = None;
      price = type_data |> member "price" |> to_int;
      houses = 0;
      rent_list =
        List.map to_int (type_data |> member "rentlist" |> to_list);
      region = type_data |> member "region" |> sort_group;
      house_price = type_data |> member "house price" |> to_int;
      mortgaged = false;
    }

let create_railroad_property (type_data : Yojson.Basic.t) :
    property_option =
  Railroad
    {
      owner = None;
      price = type_data |> member "price" |> to_int;
      rent_list =
        List.map to_int (type_data |> member "rentlist" |> to_list);
      mortgaged = false;
    }

let create_tax_property type_data : property_option =
  Tax (type_data |> member "price" |> to_int)

let create_utility type_data =
  Utility
    {
      owner = None;
      price = type_data |> member "price" |> to_int;
      rent_list =
        List.map to_int (type_data |> member "rentlist" |> to_list);
      mortgaged = false;
    }

let create_propertytype
    (proptype : Yojson.Basic.t)
    (type_data : Yojson.Basic.t) : property_option =
  match proptype |> to_string with
  | "Go" -> Go
  | "Draw" -> Draw (type_data |> to_string)
  | "Railroad" -> create_railroad_property type_data
  | "Normal" -> create_normal_property type_data
  | "Tax" -> create_tax_property type_data
  | "Jail" -> Jail
  | "Utility" -> create_utility type_data
  | "Free" -> Free
  | "GoJail" -> GoJail
  | _ -> raise InvalidJson
(*Sorts and creates the right property variant depending on property
  type*)

let from_propertyjson json (acc : pMapType) : property PropertyMap.t =
  PropertyMap.add
    (json |> member "id" |> to_int)
    {
      name = json |> member "name" |> to_string;
      description = json |> member "description" |> to_string;
      property_type =
        create_propertytype
          (json |> member "type")
          (json |> member "type info");
    }
    acc
(*Creates a property object from a json*)

let from_json json : t =
  {
    properties =
      List.fold_right from_propertyjson
        (json |> member "properties" |> to_list)
        PropertyMap.empty;
    rr_locs = List.map to_int (json |> member "railroad locs" |> to_list);
    util_locs =
      List.map to_int (json |> member "utilities locs" |> to_list);
  }
(*Creates a game_board.t type from a json*)

let init_standard_board =
  from_json (Yojson.Basic.from_file "data/game_board.json")

let search_properties properties search_id : property =
  PropertyMap.find search_id properties

let get_prop_name { properties; _ } search_id : string =
  (search_properties properties search_id).name

let get_prop_description { properties; _ } search_id : string =
  (search_properties properties search_id).description

let update_state (board : t) (search_id : int) (prop : property) : t =
  {
    board with
    properties =
      PropertyMap.update search_id (fun _ -> Some prop) board.properties;
  }

let purchase_price ({ properties; _ } : t) (search_id : int) : int =
  match (search_properties properties search_id).property_type with
  | Normal { price; _ }
  | Utility { price; _ }
  | Railroad { price; _ } ->
      price
  | _ -> raise (InvalidProperty search_id)

let house_price ({ properties; _ } : t) (search_id : int) : int =
  match (search_properties properties search_id).property_type with
  | Normal { house_price; _ } -> house_price
  | _ -> raise (InvalidProperty search_id)

let num_house ({ properties; _ } : t) (search_id : int) =
  match (search_properties properties search_id).property_type with
  | Normal { houses; _ } -> houses
  | _ -> 0

let owner ({ properties; _ } : t) (search_id : int) =
  match (search_properties properties search_id).property_type with
  | Utility { owner; _ }
  | Railroad { owner; _ }
  | Normal { owner; _ } -> (
      match owner with
      | Player name -> name
      | None -> raise Unowned)
  | _ -> raise (InvalidProperty search_id)

let add_houses (prop_details : normal_details) (purchasedhouses : int) =
  if
    prop_details.houses + purchasedhouses
    > List.length prop_details.rent_list
  then
    {
      prop_details with
      houses = List.length prop_details.rent_list - 1;
    }
  else
    { prop_details with houses = prop_details.houses + purchasedhouses }

let buy_houses (board : t) (search_id : int) (purchasedhouses : int) =
  let relproperty = search_properties board.properties search_id in
  match relproperty.property_type with
  | Normal prop_details ->
      update_state board search_id
        {
          relproperty with
          property_type =
            Normal (add_houses prop_details purchasedhouses);
        }
  | _ -> raise (InvalidProperty search_id)

let buy_property (board : t) (search_id : int) (name : string) : t =
  let relproperty = search_properties board.properties search_id in
  match relproperty.property_type with
  | Normal prop_details -> (
      match prop_details.owner with
      | _ ->
          let updatedNormal =
            Normal { prop_details with owner = Player name }
          in
          let updated_property =
            { relproperty with property_type = updatedNormal }
          in
          update_state board search_id updated_property)
  | Railroad prop_details -> (
      match prop_details.owner with
      | _ ->
          let updatedRailroad =
            Railroad { prop_details with owner = Player name }
          in
          let updated_property =
            { relproperty with property_type = updatedRailroad }
          in
          update_state board search_id updated_property)
  | Utility prop_details -> (
      match prop_details.owner with
      | _ ->
          let updatedUtility =
            Utility { prop_details with owner = Player name }
          in
          let updated_property =
            { relproperty with property_type = updatedUtility }
          in
          update_state board search_id updated_property)
  | _ -> raise (InvalidProperty search_id)

let change_mortgage (board : t) (search_id : int) (state : bool) =
  let relproperty = search_properties board.properties search_id in
  match relproperty.property_type with
  | Normal details ->
      let up_prop_details = { details with mortgaged = state } in
      update_state board search_id
        { relproperty with property_type = Normal up_prop_details }
  | Railroad details ->
      let up_prop_details = { details with mortgaged = state } in
      update_state board search_id
        { relproperty with property_type = Railroad up_prop_details }
  | _ -> raise (InvalidProperty search_id)

let mortgage_status (board : t) (search_id : int) : bool =
  match
    (search_properties board.properties search_id).property_type
  with
  | Normal { mortgaged; _ }
  | Railroad { mortgaged; _ } ->
      mortgaged
  | _ -> raise (InvalidProperty search_id)

let find_properties properties locs =
  List.fold_left
    (fun acc y -> (PropertyMap.find y properties).property_type :: acc)
    [] locs

let compare_owner
    (owner_name : string)
    (acc : int)
    (rr_det : property_option) =
  match rr_det with
  | Railroad { owner; _ } -> (
      match owner with
      | None -> acc
      | Player name -> if name = owner_name then acc + 1 else acc)
  | _ -> raise (Invalid_argument "Was not passed a railroad")
(*Compares owner_name to the owner of the railroad associated with
  rr_det.*)

let normal_rent
    ({ rent_list; houses; owner; mortgaged; _ } : normal_details) =
  match owner with
  | Player _ ->
      if not mortgaged then Rent (List.nth rent_list houses)
      else Mortgaged
  | None -> Unowned

let railroad_rent
    ({ properties; rr_locs; _ } : t)
    ({ rent_list; owner; mortgaged; _ } : railroad_details) =
  match owner with
  | None -> Unowned
  | Player owner_name ->
      if not mortgaged then
        let rr_props = find_properties properties rr_locs in
        let n_owned =
          List.fold_left (compare_owner owner_name) 0 rr_props
        in
        Rent (List.nth rent_list (n_owned - 1))
      else Mortgaged

let utility_rent
    ({ properties; util_locs; _ } : t)
    ({ rent_list; owner; mortgaged; _ } : utility_details) =
  match owner with
  | None -> Unowned
  | Player owner_name ->
      if not mortgaged then
        let util_props = find_properties properties util_locs in
        let n_owned =
          List.fold_left (compare_owner owner_name) 0 util_props
        in
        UtilityRent (List.nth rent_list (n_owned - 1))
      else Mortgaged

let landing_action (board : t) (search_id : int) =
  match
    (search_properties board.properties search_id).property_type
  with
  | Normal n_info -> normal_rent n_info
  | Railroad rr_info -> railroad_rent board rr_info
  | Utility util_info -> utility_rent board util_info
  | GoJail -> GoJail
  | Tax n -> Tax n
  | Draw s -> DrawCard s
  | _ -> Invalid

let rec remove_player
    (board : t)
    (player_properties : int list)
    (new_owner : string) : t =
  match player_properties with
  | [] -> board
  | h :: t ->
      let updated_board = buy_property board h new_owner in
      remove_player updated_board t new_owner
