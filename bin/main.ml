open Battleship

let select_ai_mode () =
  Printf.printf "Select AI mode: 1 for Easy, 2 for Hard:\n";
  match read_line () with
  | "1" ->
      set_ai_mode Easy;
      Printf.printf "Easy mode selected.\n"
  | "2" ->
      set_ai_mode Hard;
      Printf.printf "Hard mode selected.\n"
  | _ ->
      Printf.printf "Invalid selection. Defaulting to Easy mode.\n";
      set_ai_mode Easy

let rec game_loop grid_size =
  let bombed_rows = ref [] in
  let grid1 = create_grid grid_size in
  let grid2 = create_grid grid_size in
  random_place_ships grid2;

  let print_powerups () =
    Printf.printf
      "Choose which Powerup you wish to use\n Enter 1 for: Row bomb\n";
    read_line ()
  in
  let rec powerups () =
    let choice = print_powerups () in
    match choice with
    | "1" -> (
        Printf.printf
          "Row bomb will bomb an entire row, as if you shot every cell in that \
           row. Please enter a row to bomb (ex: A) or enter 'back' to go back \
           to the previous menu\n";
        let row_choice = read_line () in
        if row_choice = "back" then powerups ()
        else
          try
            let y = char_to_index row_choice.[0] in
            if List.mem y !bombed_rows then begin
              Printf.printf "Already bombed that row!\n";
              powerups ()
            end
            else begin
              bombed_rows := y :: !bombed_rows;
              for i = 0 to grid_size - 1 do
                let result = shoot grid2 (y, i) in
                Printf.printf "%s \n" result
              done
            end
          with _ ->
            Printf.printf "Invalid row input, try again";
            powerups ())
    | "back" -> Printf.printf "going back\n"
    | _ ->
        Printf.printf "invalid powerup selection\n";
        powerups ()
  in

  let rec place_ships count max_ships =
    if count < max_ships then begin
      Printf.printf "Place your %d. ship (Format: Y1X1 Y2X2, e.g., A1 A2):\n"
        (count + 1);
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
                if count + 1 = max_ships then
                  print_grid grid1 true "Final Player's Grid";
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
      shoot_phase ()
    end
  and shoot_phase () =
    Printf.printf "Enter coordinates to shoot at (Format: Y X, e.g., B3):\n";
    Printf.printf "or enter 'Powerup' to use a powerup:";
    try
      let input = read_line () in
      if String.trim input = "powerup" || input = "p" then begin
        powerups ();
        print_grid grid2 false "Opponent's Grid";
        if not (check_game_over grid1 || check_game_over grid2) then (
          let ai_result = ai_guess grid1 in
          Printf.printf "AI's move: %s\n" ai_result;
          print_grid grid1 true "Player's Grid";
          shoot_phase ())
        else if check_game_over grid1 then game_over ()
        else next_level ()
      end
      else
        let y_char = input.[0] in
        let x_substr = String.sub input 1 (String.length input - 1) in
        let y = char_to_index y_char in
        let x = int_of_string x_substr - 1 in
        if validate_coordinates y x grid_size then (
          let result = shoot grid2 (y, x) in
          Printf.printf "Result: %s\n" result;
          print_grid grid2 false "Opponent's Grid";
          if not (check_game_over grid1 || check_game_over grid2) then (
            let ai_result = ai_guess grid1 in
            Printf.printf "AI's move: %s\n" ai_result;
            print_grid grid1 true "Player's Grid";
            shoot_phase ())
          else if check_game_over grid1 then game_over ()
          else next_level ())
        else Printf.printf "Coordinates are out of bounds, try again.\n";
        shoot_phase ()
    with Scanf.Scan_failure _ | Failure _ ->
      Printf.printf "Invalid input format, try again.\n";
      shoot_phase ()
  in

  place_ships 0 1

and next_level () =
  Printf.printf "Do you want to progress to level 2? (y/n):\n";
  match read_line () with
  | "y" ->
      Printf.printf "Starting level 2...\n";
      game_loop 12
  | "n" -> main_menu ()
  | _ ->
      Printf.printf "Invalid input. Please type 'y' or 'n'.\n";
      next_level ()

and game_over () =
  Printf.printf "Game over! You have lost.\n";
  Printf.printf "Would you like to return to the main menu (1) or quit (2)?\n";
  match read_line () with
  | "1" -> main_menu ()
  | "2" -> exit 0
  | _ ->
      Printf.printf "Invalid option. Please choose 1 or 2.\n";
      game_over ()

and main_menu () =
  Printf.printf "1. Start Game\n2. Quit\nChoose an option:\n";
  match read_line () with
  | "1" ->
      select_ai_mode ();
      Printf.printf "Starting level 1...\n";
      game_loop 10
  | "2" -> exit 0
  | _ ->
      Printf.printf "Invalid option. Please choose 1 or 2.\n";
      main_menu ()

let () =
  Random.self_init ();
  main_menu ()
