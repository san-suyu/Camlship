open Battleship
open Battleship
open Bogue
module W = Widget
module L = Layout

let w = 70
let h = 70
let blue = Style.(of_bg (opaque_bg Draw.(find_color "red")))

let red =
  Style.(
    of_bg (opaque_bg Draw.(find_color "red"))
    |> with_border (mk_border (mk_line ~color:(255, 50, 140, 100) ~width:2 ())))

let gray = Style.(of_bg (opaque_bg Draw.(find_color "gray")))
let green = Style.(of_bg (opaque_bg Draw.(find_color "green")))

let bg =
  Style.(
    of_bg (opaque_bg Draw.(find_color "silver"))
    |> with_border (mk_border (mk_line ~color:(89, 89, 89, 58) ~width:2 ())))

let lbg =
  Style.(
    of_bg (opaque_bg Draw.(find_color "gainsboro"))
    |> with_border (mk_border (mk_line ~color:(183, 183, 183, 58) ~width:2 ())))

let make_style = function
  | Empty -> Style.empty
  | Ship -> blue
  | Hit -> green
  | Miss -> red

let make_widgets (a : cell array array) =
  let make_row n =
    Array.init n (fun _ -> W.box ~w ~h ~style:(make_style Empty) ())
  in
  Array.init (Array.length a) (fun _ -> make_row (Array.length a))

let make_layout ws =
  ws
  |> Array.mapi (fun i row ->
         row
         |> Array.mapi (fun j box ->
                let background =
                  if (i + j) mod 2 = 0 then L.style_bg bg else L.style_bg lbg
                in
                L.resident ~background box))
  |> Array.to_list
  |> List.map (fun row -> L.flat ~margins:0 (Array.to_list row))
  |> L.tower ~margins:0

let get_state box =
  let s = Box.get_style (W.get_box box) in
  if s = gray then Empty
  else if s = blue then Ship
  else if s = green then Hit
  else if s = red then Miss
  else failwith "Unrecognized state style"


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

let rec quit_game ask =
  if ask = "Quit" || ask = "quit" then (
    print_endline "Do you want to quit the game?";
    match read_line () with
    | input ->
        let quit = yes_no input in
        if quit = Some true then failwith "Game Exited"
        else if quit = Some false then (
          print_endline "Continuing...";
          Some true)
        else quit_game "Quit")
  else None

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
          if quit_game input == Some true then place_ships count
          else
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
                    "Invalid placement. Ships must be placed in a straight \
                     line on the grid and must not overlap with other ships."
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
                         line on the grid and must not overlap with other \
                         ships."
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
  
let init =
  Random.self_init ();
  let grid_size = 10 in
  let grid1 = create_grid grid_size in
  let grid2 = create_grid grid_size in
  random_placement grid2 5 4;
  (* game_loop grid1 grid2; *)
  let ws1 = make_widgets grid1 in
  let ws2 = make_widgets grid2 in
  let board = L.tower [ make_layout ws1; make_layout ws2 ] in
  let bog = Bogue.of_layout board in
  Bogue.run bog

let () =
  init;
  Bogue.quit ()