type name = string

type pos_id = int

type money = int

type property_id = pos_id

type t = {
  position : pos_id;
  name : name;
  money : money;
  properties : property_id list;
}

let get_name t : name = t.name

let get_pos t : pos_id = t.position

let get_money t : money = t.money

let get_properties t : property_id list = t.properties

let init_player name : t =
  { position = 0; name; money = 1500; properties = [] }

let move_forward num t : t =
  let raw_pos = num + t.position in
  let new_pos = raw_pos mod 40 in
  { t with position = new_pos }

let move_to_pos id t : t = { t with position = id }

let change_money amt t : t =
  let new_total = t.money + amt in
  { t with money = new_total }

let add_property property t : t =
  let new_properties = property :: t.properties in
  { t with properties = new_properties }

let rec add_properties properties player : t =
  match properties with
  | [] -> player
  | h :: t -> add_properties t (add_property h player)

let remove_property property t : t =
  let new_properties =
    List.filter (fun x -> x != property) t.properties
  in
  { t with properties = new_properties }

let rec remove_properties properties player : t =
  match properties with
  | [] -> player
  | h :: t -> remove_properties t (remove_property h player)
