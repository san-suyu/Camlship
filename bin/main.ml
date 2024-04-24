open Battleship

let print_grid grid show_ships =
  let grid_size = Array.length grid in
  Printf.printf "   ";
  for x = 0 to grid_size - 1 do
    Printf.printf "%2d " x
  done;
  print_newline ();
  for y = 0 to grid_size - 1 do
    Printf.printf "%c  " (Char.chr (y + Char.code 'A'));
    for x = 0 to grid_size - 1 do
      Printf.printf "%c  "
        (match grid.(y).(x) with
        | Empty -> '.'
        | Ship -> if show_ships then '#' else '.'
        | Hit -> 'X'
        | Miss -> 'O')
    done;
    print_newline ()
  done;
  print_newline ()

let check_game_over grid =
  Array.for_all
    (fun row ->
      Array.for_all
        (function
          | Ship -> false
          | _ -> true)
        row)
    grid

let shoot grid (y, x) =
  match grid.(y).(x) with
  | Ship ->
      grid.(y).(x) <- Hit;
      "Hit!"
  | Empty ->
      grid.(y).(x) <- Miss;
      "Miss!"
  | Hit | Miss -> "Already guessed this position!"

let ai_guess grid =
  let grid_size = Array.length grid in
  let x = Random.int grid_size and y = Random.int grid_size in
  shoot grid (y, x)

let game_loop grid1 grid2 =
  let max_ships = 5 in
  let rec place_ships count =
    if count < max_ships then begin
      print_endline "Opponent's grid (hidden):";
      print_grid grid2 false;
      print_endline "Your grid:";
      print_grid grid1 true;
      print_endline "Enter coordinates to place your ship (Y1 X1 Y2 X2):";
      match read_line () with
      | exception End_of_file -> ()
      | input -> (
          let coords =
            Scanf.sscanf input "%c %d %c %d" (fun y1 x1 y2 x2 ->
                ((char_to_index y1, x1), (char_to_index y2, x2)))
          in
          match place_ship grid1 (fst coords) (snd coords) with
          | true ->
              let () = print_endline "Ship placed successfully!" in
              place_ships (count + 1)
          | false ->
              let () =
                print_endline
                  "Invalid placement. Ships must be placed in a straight line \
                   on the grid and must not overlap with other ships."
              in
              place_ships count
          | exception Invalid_argument _ -> (
              match place_ship grid1 (snd coords) (fst coords) with
              | true ->
                  let () = print_endline "Ship placed successfully!" in
                  place_ships (count + 1)
              | false ->
                  let () =
                    print_endline
                      "Invalid placement. Ships must be placed in a straight \
                       line on the grid and must not overlap with other ships."
                  in
                  place_ships count))
    end
    else shoot_phase ()
  and shoot_phase () =
    print_endline "Opponent's grid (guess phase):";
    print_grid grid2 false;
    print_endline "Your grid:";
    print_grid grid1 true;
    print_endline "Enter coordinates to shoot at (Y X):";
    match read_line () with
    | exception End_of_file -> ()
    | input ->
        let coords =
          Scanf.sscanf input "%c %d" (fun y x -> (char_to_index y, x))
        in
        let result = shoot grid2 coords in
        Printf.printf "%s\n" result;
        let ai_result = ai_guess grid1 in
        Printf.printf "AI's turn: %s\n" ai_result;
        if (not (check_game_over grid1)) && not (check_game_over grid2) then
          shoot_phase ()
        else print_endline "Game over! All ships have been hit."
  in
  place_ships 0

let () =
  Random.self_init ();
  let grid_size = 10 in
  let grid1 = create_grid grid_size in
  let grid2 = create_grid grid_size in
  random_placement grid2 5 4;
  game_loop grid1 grid2
