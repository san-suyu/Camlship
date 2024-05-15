type custom_ship = {
  id : int;
  cells : (int * int) list;
  health : int;
  top_left : int * int;
}

type cell =
  | Empty
  | Ship of int
  | Mine
  | Hit of int
  | Exploded
  | Miss
  | CustomShip of custom_ship
  | HitCustom of custom_ship

type grid = cell array array

exception InvalidPlacement
exception InvalidInput

type ai_state =
  | Searching
  | Targeting of (int * int) * (int * int) list

type ai_mode =
  | Easy
  | Hard

let ai_mode : ai_mode ref = ref Easy
let set_ai_mode mode = ai_mode := mode
let get_ai_mode () = !ai_mode
let ai_memory : ai_state ref = ref Searching
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
        | Ship _ | CustomShip _ -> if show_ships then '#' else '.'
        | Mine -> if show_ships then 'M' else '.'
        | Hit _ | HitCustom _ -> 'X'
        | Exploded -> 'm'
        | Miss -> 'O'
      in
      Printf.printf "%c  " cell_repr
    done;
    print_newline ()
  done;
  print_newline ()

let validate_coordinates x y size = x >= 0 && x < size && y >= 0 && y < size
let validate_bomb x y size = x >= 0 && x < size - 2 && y >= 0 && y < size - 2
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

let place_mine grid (y, x) =
  if not (validate_coordinates x y (Array.length grid)) then
    raise InvalidPlacement
  else if grid.(y).(x) = Empty then begin
    grid.(y).(x) <- Mine;
    true
  end
  else raise InvalidPlacement

let shoot grid (y, x) =
  match grid.(y).(x) with
  | Ship id ->
      grid.(y).(x) <- Hit id;
      let health = Hashtbl.find ship_health id - 1 in
      Hashtbl.replace ship_health id health;
      if health = 0 then "You sunk a ship!" else "Hit!"
  | Mine ->
      grid.(y).(x) <- Exploded;
      "Mine hit!"
  | CustomShip custom_ship ->
      grid.(y).(x) <- HitCustom custom_ship;
      let health = custom_ship.health - 1 in
      Hashtbl.replace ship_health custom_ship.id health;
      if health = 0 then "You sunk a custom ship!" else "Hit!"
  | Empty ->
      grid.(y).(x) <- Miss;
      "Miss!"
  | Hit _ | Miss | Exploded | HitCustom _ -> "Already guessed this position!"

let rec mine_shot grid =
  let grid_size = Array.length grid in
  let x = Random.int grid_size and y = Random.int grid_size in
  begin
    match grid.(y).(x) with
    | Hit _ | Miss | Exploded -> mine_shot grid
    | _ -> shoot grid (y, x)
  end

let next_targets (x, y) grid =
  [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  |> List.filter (fun (nx, ny) ->
         validate_coordinates nx ny (Array.length grid))
  |> List.filter (fun (nx, ny) ->
         match grid.(ny).(nx) with
         | Empty | Ship _ | CustomShip _ -> true
         | Hit _ | Miss | Mine | Exploded | HitCustom _ -> false)

let rec ai_guess grid =
  match (!ai_mode, !ai_memory) with
  | Easy, _ ->
      let grid_size = Array.length grid in
      let x = Random.int grid_size and y = Random.int grid_size in
      begin
        match grid.(y).(x) with
        | Hit _ | Miss | Exploded -> ai_guess grid
        | _ -> shoot grid (y, x)
      end
  | Hard, Searching ->
      let grid_size = Array.length grid in
      let x = Random.int grid_size and y = Random.int grid_size in
      begin
        match grid.(y).(x) with
        | Hit _ | Miss | Exploded -> ai_guess grid
        | _ ->
            let result = shoot grid (y, x) in
            if result = "Hit!" then
              ai_memory := Targeting ((x, y), next_targets (x, y) grid);
            result
      end
  | Hard, Targeting ((_, _), []) ->
      ai_memory := Searching;
      ai_guess grid
  | Hard, Targeting ((last_hit_x, last_hit_y), targets) -> (
      match targets with
      | (target_x, target_y) :: rest ->
          let result = shoot grid (target_y, target_x) in
          if result = "Hit!" then
            ai_memory :=
              Targeting
                ( (target_x, target_y),
                  next_targets (target_x, target_y) grid @ rest )
          else ai_memory := Targeting ((last_hit_x, last_hit_y), rest);
          result
      | [] ->
          ai_memory := Searching;
          ai_guess grid)

let random_place_ships grid =
  let ship_sizes = [ 5; 4; 3; 3; 2 ] in
  let grid_size = Array.length grid in
  List.iteri
    (fun ship_id size ->
      let placed = ref false in
      while not !placed do
        let dir = Random.bool () in
        let x = Random.int (if dir then grid_size - size else grid_size) in
        let y = Random.int (if dir then grid_size else grid_size - size) in
        let x2, y2 = if dir then (x + size - 1, y) else (x, y + size - 1) in
        try placed := place_ship grid ship_id (y, x) (y2, x2)
        with InvalidPlacement -> ()
      done)
    ship_sizes

let check_game_over grid =
  let all_empty = ref true in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid - 1 do
      if grid.(y).(x) != Empty then all_empty := false else ()
    done
  done;
  if !all_empty then false
  else
    Array.for_all
      (fun row ->
        Array.for_all
          (function
            | Ship _ -> false
            | _ -> true)
          row)
      grid

let count_cell_type grid (cell : cell) =
  let count = ref 0 in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid - 1 do
      if cell = grid.(y).(x) then count := !count + 1
    done
  done;
  !count

let count_hit_cells grid =
  let count = ref 0 in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid - 1 do
      match grid.(y).(x) with
      | Hit _ -> count := !count + 1
      | _ -> ()
    done
  done;
  !count

let calculate_top_left cells =
  List.fold_left
    (fun (min_y, min_x) (y, x) -> (min min_y y, min min_x x))
    (max_int, max_int) cells

let assemble_custom_ship pieces id =
  let cells = List.flatten pieces in
  let health = List.length cells in
  let top_left = calculate_top_left cells in
  { id; cells; health; top_left }

let place_custom_ship grid custom_ship top_left =
  let offset_cells =
    List.map
      (fun (y, x) -> (y + fst top_left, x + snd top_left))
      custom_ship.cells
  in
  if
    List.for_all
      (fun (y, x) -> validate_coordinates y x (Array.length grid))
      offset_cells
  then (
    let place_coordinate (y, x) =
      if grid.(y).(x) = Empty then grid.(y).(x) <- CustomShip custom_ship
      else raise InvalidPlacement
    in
    try
      List.iter place_coordinate offset_cells;
      Hashtbl.add ship_health custom_ship.id custom_ship.health;
      true
    with InvalidPlacement ->
      List.iter
        (fun (y, x) ->
          if grid.(y).(x) = CustomShip custom_ship then grid.(y).(x) <- Empty)
        offset_cells;
      raise InvalidPlacement)
  else raise InvalidPlacement

let create_custom_ship_from_grid grid =
  let coordinates = ref [] in
  let health = ref 0 in
  let min_x = ref max_int and min_y = ref max_int in
  let max_x = ref min_int and max_y = ref min_int in
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid.(0) - 1 do
      match grid.(y).(x) with
      | Ship _ ->
          coordinates := (y, x) :: !coordinates;
          health := !health + 1;
          if x < !min_x then min_x := x;
          if y < !min_y then min_y := y;
          if x > !max_x then max_x := x;
          if y > !max_y then max_y := y
      | _ -> ()
    done
  done;
  let new_coordinates =
    List.map (fun (y, x) -> (y - !min_y, x - !min_x)) !coordinates
  in
  {
    id = 0;
    cells = new_coordinates;
    health = !health;
    top_left = (!min_y, !min_x);
  }

let rec read_coordinates grid =
  Printf.printf
    "Enter coordinates for the ship (Format: YX YX, e.g., A1 A2) or type \
     'done' to finish:\n";
  match String.lowercase_ascii (read_line ()) with
  | "done" -> create_custom_ship_from_grid grid
  | input ->
      let inputs = String.split_on_char ' ' input in
      let rec process_coords = function
        | [] -> ()
        | coord :: rest -> (
            try
              let y = char_to_index coord.[0] in
              let x =
                int_of_string (String.sub coord 1 (String.length coord - 1)) - 1
              in
              if validate_coordinates y x (Array.length grid) then (
                grid.(y).(x) <- Ship 0;
                (* Use ship_id = 0 for custom ship pieces *)
                process_coords rest)
              else (
                Printf.printf "Invalid coordinates, try again.\n";
                process_coords rest)
            with _ ->
              Printf.printf "Invalid input format, try again.\n";
              process_coords rest)
      in
      process_coords inputs;
      print_grid grid true "Custom Ship Design";
      read_coordinates grid

let get_ship_health_length () = Hashtbl.length ship_health
