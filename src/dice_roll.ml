type roll = int

(* let first_dice_roll roll = Random.int roll

   let second_dice_roll roll = first_dice_roll roll *)

let two_rolls max =
  let () = Unix.time () |> Float.to_int |> Random.init in
  (Random.int max + 1, Random.int max + 1)

let total_roll (two_rolls : roll * roll) =
  match two_rolls with
  | x, y -> x + y

let is_snake_eyes (two_rolls : roll * roll) =
  match two_rolls with
  | 1, 1 -> true
  | _, _ -> false

let pretty_print_dice (n1, n2) : unit =
  (* let b = Uchar.(of_int 2022 |> to_char |> Char.escaped) in *)
  let b = "." in
  let to_grid = function
    | 1 -> ((" ", " ", " "), (" ", b, " "), (" ", " ", " "))
    | 2 -> ((b, " ", " "), (" ", " ", " "), (" ", " ", b))
    | 3 -> ((b, " ", " "), (" ", b, " "), (" ", " ", b))
    | 4 -> ((b, " ", b), (" ", " ", " "), (b, " ", b))
    | 5 -> ((b, " ", b), (" ", b, " "), (b, " ", b))
    | 6 -> ((b, " ", b), (b, " ", b), (b, " ", b))
    | _ -> raise (Invalid_argument (string_of_int n1))
  in
  let (a1, a2, a3), (a4, a5, a6), (a7, a8, a9) = to_grid n1 in
  let (b1, b2, b3), (b4, b5, b6), (b7, b8, b9) = to_grid n2 in
  print_endline "_______  _______";
  print_endline
    ("|" ^ a1 ^ " " ^ a2 ^ " " ^ a3 ^ "|  |" ^ b1 ^ " " ^ b2 ^ " " ^ b3
   ^ "|");
  print_endline
    ("|" ^ a4 ^ " " ^ a5 ^ " " ^ a6 ^ "|  |" ^ b4 ^ " " ^ b5 ^ " " ^ b6
   ^ "|");
  ANSITerminal.print_string [ Underlined ]
    ("|" ^ a7 ^ " " ^ a8 ^ " " ^ a9 ^ "|");
  print_string "  ";
  ANSITerminal.print_string [ Underlined ]
    ("|" ^ b7 ^ " " ^ b8 ^ " " ^ b9 ^ "|");
  print_string "\n \n"
