open Battleship

let () =
  let grid_size = 10 in
  let grid = create_grid grid_size in
  print_endline "Welcome to Battleship!";

  let max_ships = 5 in
  (* Define the maximum number of ships allowed *)
  let rec game_loop ship_count =
    if ship_count < max_ships then begin
      print_endline "Enter coordinates for the ship (x1 y1 x2 y2):";
      match read_line () with
      | exception End_of_file -> ()
      | input ->
          let coords =
            Scanf.sscanf input "%d %d %d %d" (fun x1 y1 x2 y2 ->
                ((x1, y1), (x2, y2)))
          in
          if place_ship grid (fst coords) (snd coords) then begin
            print_endline "Ship placed successfully!";
            game_loop (ship_count + 1) (* Increment the ship count *)
          end
          else begin
            print_endline
              "Invalid placement. Ships must be placed in a straight line on \
               the grid.";
            game_loop
              ship_count (* Do not increment the ship count on failure *)
          end
    end
    else print_endline "All ships placed. Game ready to start!"
  in
  game_loop 0
