open Battleship

let print_grid grid =
  let grid_size = Array.length grid in
  Printf.printf "  ";
  for x = 0 to grid_size - 1 do
    Printf.printf "%2d" x
  done;
  print_newline ();
  for y = 0 to grid_size - 1 do
    Printf.printf "%2d" y;
    for x = 0 to grid_size - 1 do
      Printf.printf " %c"
        (match grid.(y).(x) with
        | Empty -> '.'
        | Ship -> '#')
    done;
    print_newline ()
  done;
  print_newline ()

let () =
  let grid_size = 10 in
  let grid = create_grid grid_size in
  print_endline "Welcome to Battleship!";

  let max_ships = 5 in
  let rec game_loop ship_count =
    if ship_count < max_ships then begin
      print_grid grid;
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
            game_loop (ship_count + 1)
          end
          else begin
            print_endline
              "Invalid placement. Ships must be placed in a straight line on \
               the grid.";
            game_loop ship_count
          end
    end
    else begin
      print_grid grid;
      print_endline "All ships placed. Game ready to start!"
    end
  in
  game_loop 0
