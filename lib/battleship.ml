type cell =
  | Empty
  | Ship of int
  | Hit of int
  | Miss

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

let shoot grid (y, x) =
  match grid.(y).(x) with
  | Ship id ->
      grid.(y).(x) <- Hit id;
      let health = Hashtbl.find ship_health id - 1 in
      Hashtbl.replace ship_health id health;
      if health = 0 then "You sunk a ship!" else "Hit!"
  | Empty ->
      grid.(y).(x) <- Miss;
      "Miss!"
  | Hit _ | Miss -> "Already guessed this position!"

let next_targets (x, y) grid =
  [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
  |> List.filter (fun (nx, ny) ->
         validate_coordinates nx ny (Array.length grid))
  |> List.filter (fun (nx, ny) ->
         match grid.(ny).(nx) with
         | Empty | Ship _ -> true
         | Hit _ | Miss -> false)

let rec ai_guess grid =
  match (!ai_mode, !ai_memory) with
  | Easy, _ ->
      let grid_size = Array.length grid in
      let x = Random.int grid_size and y = Random.int grid_size in
      begin
        match grid.(y).(x) with
        | Hit _ | Miss -> ai_guess grid
        | _ -> shoot grid (y, x)
      end
  | Hard, Searching ->
      let grid_size = Array.length grid in
      let x = Random.int grid_size and y = Random.int grid_size in
      begin
        match grid.(y).(x) with
        | Hit _ | Miss -> ai_guess grid
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
  Array.for_all
    (fun row ->
      Array.for_all
        (function
          | Ship _ -> false
          | _ -> true)
        row)
    grid
