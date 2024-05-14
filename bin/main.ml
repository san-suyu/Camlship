open Battleship

let mode = ref 0

let select_ai_mode () =
  Printf.printf
    "Select AI mode: 1 for Easy, 2 for Hard, 3 for Two player mode:\n";
  match read_line () with
  | "1" ->
      set_ai_mode Easy;
      Printf.printf "Easy mode selected.\n";
      mode := !mode + 1
  | "2" ->
      set_ai_mode Hard;
      Printf.printf "Hard mode selected.\n";
      mode := !mode + 2
  | "3" ->
      Printf.printf "Two player mode selected\n";
      mode := !mode + 3
  | _ ->
      Printf.printf "Invalid selection. Defaulting to Easy mode.\n";
      set_ai_mode Easy;
      mode := !mode + 1

let gold1 = ref 100
let gold2 = ref 100

let rec game_loop grid_size =
  let bombed_rows1 = ref [] in
  let bombed_columns1 = ref [] in
  let bombed_rows2 = ref [] in
  let bombed_columns2 = ref [] in
  let bombed_squares1 = ref [] in
  let bombed_squares2 = ref [] in
  let grid1 = create_grid grid_size in
  let grid2 = create_grid grid_size in
  let grid3 = create_grid grid_size in
  random_place_ships grid2;

  let print_powerups () =
    ANSITerminal.printf [ ANSITerminal.yellow ]
      "You currently have %i gold to spend\n" !gold1;
    Printf.printf
      "Choose which Powerup you wish to use\n\
      \ Enter 1 for: Row bomb -> 100g\n\
      \ Enter 2 for: Column bomb -> 100g\n\
      \ Enter 3 for: Square bomb -> 100g\n\
      \ Enter 4 for: Airstrike -> 50g\n\
      \       Enter Back for: back to game\n";
    String.trim (read_line ())
  in
  let rec powerups player () =
    let gold = if player = 1 then gold1 else gold2 in
    let grid = if player = 1 then grid3 else grid1 in
    let bombed_columns =
      if player = 1 then bombed_columns1 else bombed_columns2
    in
    let bombed_rows = if player = 1 then bombed_rows1 else bombed_rows2 in
    let bombed_squares =
      if player = 1 then bombed_squares1 else bombed_squares2
    in
    let choice = print_powerups () in
    match choice with
    | "1" ->
        if !gold >= 100 then (
          Printf.printf
            "Row bomb will bomb an entire row, as if you shot every cell in \
             that row. Please enter a row to bomb (ex: A) or enter 'back' to \
             go back to the previous menu\n";
          let row_choice = read_line () in
          if row_choice = "back" then powerups player ()
          else if row_choice = "Quit" then exit 0
          else
            try
              let y = char_to_index row_choice.[0] in
              if List.mem y !bombed_rows then begin
                Printf.printf "Already bombed that row!\n";
                powerups player ()
              end
              else begin
                bombed_rows := y :: !bombed_rows;
                for i = 0 to grid_size - 1 do
                  let result = shoot grid (y, i) in
                  if result = "Hit!" then gold := !gold + 50;
                  Printf.printf "%s \n" result
                done;
                gold := !gold - 100
              end
            with _ ->
              Printf.printf "Invalid row input, try again\n";
              powerups player ())
        else begin
          Printf.printf
            "Not enough gold to spend!\n\
            \ select another powerup or use 'back' to go back\n";
          powerups player ()
        end
    | "2" ->
        if !gold >= 100 then (
          Printf.printf
            "Column bomb will bomb an entire column, as if you shot every cell \
             in that column. Please enter a column to bomb (ex: 2) or enter \
             'back' to go back to the previous menu\n";
          let column_choice = read_line () in
          if column_choice = "back" then powerups player ()
          else if column_choice = "Quit" then exit 0
          else
            try
              let x = int_of_string column_choice in
              if List.mem x !bombed_columns then begin
                Printf.printf "Already bombed that column!\n";
                powerups player ()
              end
              else begin
                bombed_columns := x :: !bombed_columns;
                for i = 0 to grid_size - 1 do
                  let result = shoot grid (i, x) in
                  Printf.printf "%s \n" result
                done;
                gold := !gold - 100
              end
            with _ ->
              Printf.printf "Invalid column input, try again\n";
              powerups player ())
        else begin
          Printf.printf
            "Not enough gold to spend!\n\
            \ select another powerup or use 'back' to go back\n";
          powerups player ()
        end
    | "3" ->
        if !gold >= 100 then (
          Printf.printf
            "Square bomb will bomb a 3x3 area. Please enter the top left \
             corner of the area you wish to bomb (ex: A1) or enter 'back' to \
             go back to the previous menu\n";
          let square_choice = read_line () in
          if square_choice = "back" then powerups player ()
          else if square_choice = "Quit" then exit 0
          else
            try
              let y_char = square_choice.[0] in
              let x_substr =
                String.sub square_choice 1 (String.length square_choice - 1)
              in
              let y = char_to_index y_char in
              let x = int_of_string x_substr - 1 in
              if not (validate_bomb y x grid_size) then begin
                Printf.printf "Invalid selection!\n";
                powerups player ()
              end
              else if List.mem (y, x) !bombed_squares then begin
                Printf.printf "Already bombed that square!\n";
                powerups player ()
              end
              else begin
                bombed_squares := (y, x) :: !bombed_squares;
                for i = 0 to 2 do
                  for j = 0 to 2 do
                    let result = shoot grid2 (y + i, x + j) in
                    if result = "Hit!" then gold := !gold + 50;
                    Printf.printf "%s \n" result
                  done
                done;
                gold := !gold - 100
              end
            with _ ->
              Printf.printf "Invalid input, try again\n";
              powerups player ())
        else begin
          Printf.printf
            "Not enough gold to spend!\n\
            \ select another powerup or use 'back' to go back\n";
          powerups player ()
        end
    | "4" ->
        if !gold >= 25 then (
          Printf.printf
            "Airstrike will randomly shoot cells on the enemy board. Each 25 \
             gold you are willing to pay will shoot 1 cell. Please enter an \
             the amount of gold you are willing to spend (ex: 100) or enter \
             'back' to go back to the previous menu\n";
          let rec airstrike grid shots =
            let grid_size = Array.length grid in
            let x = Random.int grid_size and y = Random.int grid_size in
            if shots = 0 then print_string ""
            else
              let () = gold := !gold - 25 in
              match grid.(y).(x) with
              | Hit _ -> airstrike grid shots
              | _ ->
                  let result = shoot grid (y, x) in
                  if result = "Hit!" then
                    let () = gold := !gold + 50 in
                    let () = Printf.printf "%s \n" result in
                    airstrike grid (shots - 1)
                  else
                    let () = Printf.printf "%s \n" result in
                    airstrike grid (shots - 1)
          in
          let (current_gold : int option) =
            int_of_string_opt (string_of_int !gold)
          in
          let airstrike_choice = read_line () in
          if airstrike_choice = "back" then powerups player ()
          else
            let gold_input = int_of_string_opt airstrike_choice in
            if gold_input = None then
              let () = Printf.printf "Invalid input, try again\n" in
              powerups player ()
            else if gold_input > current_gold then
              let () = print_endline "You do not have enough gold!\n" in
              powerups player ()
            else
              airstrike grid2
                ((int_of_string airstrike_choice
                 - Int.rem (int_of_string airstrike_choice) 25)
                / 25))
        else begin
          Printf.printf
            "Not enough gold to spend!\n\
            \ select another powerup or use 'back' to go back\n";
          powerups player ()
        end
    | "back" -> Printf.printf "Going back\n"
    | "Quit" -> exit 0
    | _ ->
        ANSITerminal.printf [ ANSITerminal.red ] "invalid powerup selection\n";
        powerups player ()
  in
  if !mode <> 3 then begin
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
              let start_x_substr =
                String.sub start 1 (String.length start - 1)
              in
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
                match place_ship grid1 count (y1, x1) (y2, x2) with
                | true ->
                    let () = Printf.printf "Ship placed successfully.\n" in
                    let () = print_grid grid1 true "Final Player's Grid" in
                    place_ships (count + 1) max_ships
                | false ->
                    let () = Printf.printf "Invalid placement, try again.\n" in
                    place_ships count max_ships
                | exception Invalid_argument _ -> (
                    match place_ship grid1 count (y2, x2) (y1, x1) with
                    | true ->
                        let () = Printf.printf "Ship placed successfully.\n" in
                        let () = print_grid grid1 true "Final Player's Grid" in
                        place_ships (count + 1) max_ships
                    | false ->
                        let () =
                          Printf.printf "Invalid placement, try again.\n"
                        in
                        place_ships count max_ships)
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
      Printf.printf "You currently have %i gold\n" !gold1;
      Printf.printf "Enter coordinates to shoot at (Format: Y X, e.g., B3):\n";
      Printf.printf "or enter 'Powerup' to use a powerup:";
      try
        let input = read_line () in
        if String.trim input = "powerup" || input = "p" then begin
          powerups 1 ();
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

            if result = "Hit!" || result = "You sunk a ship!" then
              ANSITerminal.printf [ ANSITerminal.green ] "Result: %s\n" result
            else if result = "Miss!" then
              ANSITerminal.printf [ ANSITerminal.green ] "Result: %s\n" result;

            if result = "Hit!" then gold1 := !gold1 + 50;
            print_grid grid2 false "Opponent's Grid";
            if not (check_game_over grid1 || check_game_over grid2) then (
              let ai_result = ai_guess grid1 in
              Printf.printf "AI's move: %s\n" ai_result;
              print_grid grid1 true "Player's Grid";
              shoot_phase ())
            else if check_game_over grid1 then game_over ()
            else next_level ())
          else
            ANSITerminal.printf [ ANSITerminal.red ]
              "Coordinates are out of bounds, try again.\n";
          shoot_phase ()
      with Scanf.Scan_failure _ | Failure _ ->
        ANSITerminal.printf [ ANSITerminal.red ]
          "Invalid input format, try again.\n";
        shoot_phase ()
    in

    place_ships 0 1
  end
  else begin
    let rec player1 count max_ships =
      if count < max_ships then begin
        Printf.printf "Place your %d ship (Format: Y1X1 Y2X2, e.g., A1 A2):\n"
          (count + 1);
        print_grid grid1 true "Player 1's Grid";
        try
          let input = read_line () in
          let inputs = Str.split (Str.regexp "[ \t]+") input in
          match inputs with
          | [ start; finish ] -> begin
              let start_y_char = start.[0] in
              let start_x_substr =
                String.sub start 1 (String.length start - 1)
              in
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
                    print_grid grid1 true "Final Player 1's Grid";
                  player1 (count + 1) max_ships
                end
                else begin
                  Printf.printf "Invalid placement, try again.\n";
                  player1 count max_ships
                end
              else begin
                Printf.printf "Coordinates are out of bounds, try again.\n";
                player1 count max_ships
              end
            end
          | _ -> raise (Failure "Invalid input format")
        with
        | Scanf.Scan_failure _ | Failure _ ->
            Printf.printf "Please check your input format and try again.\n";
            player1 count max_ships
        | InvalidPlacement ->
            Printf.printf "Invalid placement, try again.\n";
            player1 count max_ships
      end
      else begin
        ANSITerminal.print_string [ ANSITerminal.red ]
          "This is your final grid, press enter to hide your grid and let the \
           other player place their ships\n";
        let _ = read_line () in
        ANSITerminal.erase Above;
        player2 0 1
      end
    and player2 count max_ships =
      if count < max_ships then begin
        Printf.printf "Place your %d. ship (Format: Y1X1 Y2X2, e.g., A1 A2):\n"
          (count + 1);
        print_grid grid3 true "Player 2's Grid";
        try
          let input = read_line () in
          let inputs = Str.split (Str.regexp "[ \t]+") input in
          match inputs with
          | [ start; finish ] -> begin
              let start_y_char = start.[0] in
              let start_x_substr =
                String.sub start 1 (String.length start - 1)
              in
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
                if place_ship grid3 count (y1, x1) (y2, x2) then begin
                  Printf.printf "Ship placed successfully.\n";
                  if count + 1 = max_ships then
                    print_grid grid3 true "Final Player 2's Grid";
                  player2 (count + 1) max_ships
                end
                else begin
                  Printf.printf "Invalid placement, try again.\n";
                  player2 count max_ships
                end
              else begin
                Printf.printf "Coordinates are out of bounds, try again.\n";
                player2 count max_ships
              end
            end
          | _ -> raise (Failure "Invalid input format")
        with
        | Scanf.Scan_failure _ | Failure _ ->
            Printf.printf "Please check your input format and try again.\n";
            player2 count max_ships
        | InvalidPlacement ->
            Printf.printf "Invalid placement, try again.\n";
            player2 count max_ships
      end
      else begin
        ANSITerminal.print_string [ ANSITerminal.red ]
          "This is your final grid, press enter to hide your grid and let the \
           other player place their ships\n";
        let _ = read_line () in
        ANSITerminal.erase Above;
        print_grid grid3 false "Opponent's Grid";
        print_grid grid1 true "Your Grid";
        ANSITerminal.print_string [ ANSITerminal.red ]
          "===================================\n";
        duo_shoot_phase 1 ()
      end
    and duo_shoot_phase player () =
      let gold = if player mod 2 = 0 then gold2 else gold1 in
      let grid = if player mod 2 = 0 then grid1 else grid3 in
      let player_no = if player mod 2 = 0 then 2 else 1 in
      ANSITerminal.printf [ ANSITerminal.magenta ]
        "It is currently Player %i 's turn\n" player_no;
      Printf.printf "You currently have %i gold\n" !gold;
      Printf.printf "Enter coordinates to shoot at (Format: Y X, e.g., B3):\n";
      Printf.printf "or enter 'Powerup' to use a powerup:";
      try
        let input = read_line () in
        if String.trim input = "powerup" || input = "p" then begin
          powerups player_no ();
          print_grid grid3 false "Opponent's Grid";
          if not (check_game_over grid1 || check_game_over grid3) then
            if player_no = 1 then begin
              ANSITerminal.erase Above;
              ANSITerminal.printf [ ANSITerminal.magenta ] "Player 2's move\n";
              print_grid grid1 false "Opponent's grid";
              print_grid grid true "Your Grid";

              duo_shoot_phase (player + 1) ()
            end
            else begin
              ANSITerminal.erase Above;
              ANSITerminal.printf [ ANSITerminal.magenta ] "Player 1's move\n";
              print_grid grid3 false "Opponent's grid";
              print_grid grid true "Your Grid";

              duo_shoot_phase (player + 1) ()
            end
          else if check_game_over grid then duo_game_over 2 ()
          else duo_game_over 1 ()
        end
        else
          let y_char = input.[0] in
          let x_substr = String.sub input 1 (String.length input - 1) in
          let y = char_to_index y_char in
          let x = int_of_string x_substr - 1 in
          if validate_coordinates y x grid_size then (
            let result = shoot grid (y, x) in

            if result = "Hit!" || result = "You sunk a ship!" then
              ANSITerminal.printf [ ANSITerminal.green ] "Result: %s\n" result
            else if result = "Miss!" then
              ANSITerminal.printf [ ANSITerminal.green ] "Result: %s\n" result;

            if result = "Hit!" then gold := !gold + 50;
            if not (check_game_over grid1 || check_game_over grid3) then
              if player_no = 1 then begin
                ANSITerminal.erase Above;
                ANSITerminal.printf [ ANSITerminal.magenta ] "Player 2's move\n";
                print_grid grid1 false "Opponent's grid";
                print_grid grid true "Your Grid";

                duo_shoot_phase (player + 1) ()
              end
              else begin
                ANSITerminal.erase Above;
                ANSITerminal.printf [ ANSITerminal.magenta ] "Player 1's move\n";
                print_grid grid3 false "Opponent's grid";
                print_grid grid true "Your Grid";

                duo_shoot_phase (player + 1) ()
              end
            else if check_game_over grid1 then duo_game_over 2 ()
            else duo_game_over 1 ())
          else
            ANSITerminal.printf [ ANSITerminal.red ]
              "Coordinates are out of bounds, try again.\n";
          duo_shoot_phase player ()
      with Scanf.Scan_failure _ | Failure _ ->
        ANSITerminal.printf [ ANSITerminal.red ]
          "Invalid input format, try again.\n";
        duo_shoot_phase player ()
    in

    player1 0 1
  end

and next_level () =
  Printf.printf "You won!\n Do you want to progress to level 2? (y/n):\n";
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

and duo_game_over player () =
  Printf.printf "Game over! Player %i has won!\n" player;
  Printf.printf "Would you like to return to the main menu (1) or quit (2)?\n";
  match read_line () with
  | "1" -> main_menu ()
  | "2" -> exit 0
  | _ ->
      Printf.printf "Invalid option. Please choose 1 or 2.\n";
      duo_game_over player ()

and main_menu () =
  Printf.printf "Welcome to battleship!\n";
  Printf.printf "1. Start Game\n2. Quit\nChoose an option:\n";
  match read_line () with
  | "1" ->
      select_ai_mode ();
      Printf.printf
        "Starting level 1...\nYou may type 'Quit' at any time to exit.\n";
      game_loop 10
  | "2" -> exit 0
  | _ ->
      Printf.printf "Invalid option. Please choose 1 or 2.\n";
      main_menu ()

let () =
  Random.self_init ();
  main_menu ()
