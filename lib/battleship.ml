type cell =
  | Empty
  | Ship of int
  | Hit of int
  | Miss

type grid = cell array array

exception InvalidPlacement
exception InvalidInput

let create_grid size = Array.make_matrix size size Empty

let print_grid grid show_ships title =
  Printf.printf "%s\n" title;
  let grid_size = Array.length grid in
  Printf.printf "  ";
  for x = 1 to grid_size do
    Printf.printf "%2d " x
  done;
  print_newline ();
  for y = 0 to grid_size - 1 do
    Printf.printf "%c  " (Char.chr (y + Char.code 'A'));
    for x = 0 to grid_size - 1 do
      let cell_repr =
        match grid.(y).(x) with
        | Empty -> '.'
        | Ship _ -> if show_ships then '#' else '.'
        | Hit _ -> 'X'
        | Miss -> 'O'
      in
      Printf.printf "%c  " cell_repr
    done;
    print_newline ()
  done;
  print_newline ()

let validate_coordinates x y size = x >= 0 && x < size && y >= 0 && y < size
let char_to_index c = Char.code (Char.uppercase_ascii c) - Char.code 'A'

let is_valid_placement (y1, x1) (y2, x2) =
  let horizontal = x1 = x2 && abs (y2 - y1) > 0 in
  let vertical = y1 = y2 && abs (x2 - x1) > 0 in
  horizontal || vertical

let ship_health = Hashtbl.create 10

let place_ship grid ship_id (y1, x1) (y2, x2) =
  if
    (not (validate_coordinates x1 y1 (Array.length grid)))
    || not (validate_coordinates x2 y2 (Array.length grid))
  then raise InvalidPlacement
  else if not (is_valid_placement (y1, x1) (y2, x2)) then raise InvalidPlacement
  else
    let dir = if y1 = y2 then `Horizontal else `Vertical in
    let length = max (abs (y2 - y1)) (abs (x2 - x1)) + 1 in
    let generate_coords =
      match dir with
      | `Horizontal -> List.init length (fun i -> (y1, x1 + i))
      | `Vertical -> List.init length (fun i -> (y1 + i, x1))
    in
    if List.for_all (fun (y, x) -> grid.(y).(x) = Empty) generate_coords then begin
      List.iter (fun (y, x) -> grid.(y).(x) <- Ship ship_id) generate_coords;
      Hashtbl.add ship_health ship_id length;
      true
    end
    else raise InvalidPlacement

let shoot grid (y, x) =
  match grid.(y).(x) with
  | Ship id ->
      grid.(y).(x) <- Hit id;
      let health = Hashtbl.find ship_health id - 1 in
      Hashtbl.replace ship_health id health;
      if health = 0 then Printf.sprintf "You sunk a ship!" else "Hit!"
  | Empty ->
      grid.(y).(x) <- Miss;
      "Miss!"
  | Hit _ | Miss -> "Already guessed this position!"

let ai_guess grid =
  let grid_size = Array.length grid in
  let rec guess () =
    let x = Random.int grid_size and y = Random.int grid_size in
    match grid.(y).(x) with
    | Hit _ | Miss -> guess ()
    | _ -> shoot grid (y, x)
  in
  guess ()

let check_game_over grid =
  Array.for_all
    (fun row ->
      Array.for_all
        (function
          | Ship _ -> false
          | _ -> true)
        row)
    grid
