open Battleship

(* Checks if coordinates are within the acceptable range of the grid *)
let validate_coordinates y x size =
  let valid_y = y >= 0 && y < size in
  let valid_x = x >= 0 && x < size in
  valid_y && valid_x

(* Initializes random placement of ships with specified sizes *)
let random_place_ships grid =
  let ship_sizes = [ 5; 4; 3; 3; 2 ] in
  let try_place_ship size =
    let direction = Random.bool () in
    let dir = if direction then `Horizontal else `Vertical in
    let decrement = if dir = `Horizontal then size else 0 in
    let x = Random.int (10 - decrement) in
    let decrement_y = if dir = `Vertical then size else 0 in
    let y = Random.int (10 - decrement_y) in
    let x2 = if dir = `Horizontal then x + size - 1 else x in
    let y2 = if dir = `Vertical then y + size - 1 else y in
    let valid_start = validate_coordinates x y 10 in
    let valid_end = validate_coordinates x2 y2 10 in
    if valid_start && valid_end then place_ship grid (size + 10) (y, x) (y2, x2)
    else false
  in
  List.iter
    (fun size ->
      while not (try_place_ship size) do
        ()
      done)
    ship_sizes

let game_loop grid1 grid2 =
  let grid_size = 10 in
  random_place_ships grid2;
  let rec place_ships count max_ships =
    if count < max_ships then begin
      Printf.printf "Place your %d. ship (Format: Y1X1 Y2X2, e.g., A1 A2): \n"
        (count + 1);
      print_grid grid2 false "Opponent's Grid";
      print_grid grid1 true "Player's Grid";
      try
        let input = read_line () in
        let inputs = Str.split (Str.regexp "[ \t]+") input in
        match inputs with
        | [ start; finish ] -> begin
            let start_y_char = start.[0] in
            let start_x_substr = String.sub start 1 (String.length start - 1) in
            let y1 = char_to_index start_y_char in
            let x1 = int_of_string start_x_substr - 1 in
            let finish_y_char = finish.[0] in
            let finish_x_substr =
              String.sub finish 1 (String.length finish - 1)
            in
            let y2 = char_to_index finish_y_char in
            let x2 = int_of_string finish_x_substr - 1 in
            if
              validate_coordinates y1 x1 grid_size
              && validate_coordinates y2 x2 grid_size
            then
              if place_ship grid1 count (y1, x1) (y2, x2) then begin
                Printf.printf "Ship placed successfully.\n";
                print_grid grid1 true "Player's Grid";
                place_ships (count + 1) max_ships
              end
              else begin
                Printf.printf "Invalid placement, try again.\n";
                place_ships count max_ships
              end
            else begin
              Printf.printf "Coordinates are out of bounds, try again.\n";
              place_ships count max_ships
            end
          end
        | _ -> raise (Failure "Invalid input format")
      with
      | Scanf.Scan_failure _ | Failure _ ->
          Printf.printf "Please check your input format and try again.\n";
          place_ships count max_ships
      | InvalidPlacement ->
          Printf.printf "Invalid placement, try again.\n";
          place_ships count max_ships
    end
    else begin
      print_grid grid2 false "Opponent's Grid";
      Printf.printf "Transitioning to shoot phase.\n";
      shoot_phase ()
    end
  and shoot_phase () =
    Printf.printf "Enter coordinates to shoot at (Format: Y X, e.g., B3): \n";
    try
      let input = read_line () in
      let y_char = input.[0] in
      let x_substr = String.sub input 1 (String.length input - 1) in
      let y = char_to_index y_char in
      let x = int_of_string x_substr - 1 in
      if validate_coordinates y x grid_size then (
        let result = shoot grid2 (y, x) in
        Printf.printf "Result: %s\n" result;
        print_grid grid2 false "Opponent's Grid";
        print_grid grid1 true "Player's Grid";
        if not (check_game_over grid1 || check_game_over grid2) then
          let _ = ai_guess grid1 in
          shoot_phase ()
        else Printf.printf "Game over! All ships have been sunk.\n")
      else Printf.printf "Coordinates are out of bounds, try again.\n";
      shoot_phase ()
    with Scanf.Scan_failure _ | Failure _ ->
      Printf.printf "Invalid input format, try again.\n";
      shoot_phase ()
  in
  place_ships 0 5

let () =
  Random.self_init ();
  let grid1 = create_grid 10 in
  let grid2 = create_grid 10 in
  Printf.printf "Starting Battleship game\n";
  game_loop grid1 grid2
