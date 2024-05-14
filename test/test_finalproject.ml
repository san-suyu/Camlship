open OUnit2
open Battleship

let test_create_grid _ =
  let grid = create_grid 10 in
  assert_equal 10 (Array.length grid);
  assert_equal 10 (Array.length grid.(0));
  for y = 0 to Array.length grid - 1 do
    for x = 0 to Array.length grid - 1 do
      assert_equal Empty grid.(y).(x)
    done
  done

let test_print_grid _ =
  let grid = create_grid 10 in
  assert_bool "No exception on print_grid"
    (try
       print_grid grid true "Test Grid";
       true
     with _ -> false)

let test_place_ship_success _ =
  let grid = create_grid 10 in
  assert_bool "Horizontal placement should succeed"
    (place_ship grid 1 (0, 0) (0, 4));
  assert_bool "Vertical placement should succeed"
    (place_ship grid 2 (2, 2) (6, 2));
  for x = 0 to 4 do
    assert_equal (Ship 1) grid.(0).(x)
  done;
  for y = 2 to 6 do
    assert_equal (Ship 2) grid.(y).(2)
  done

let test_place_ship_failures _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 4) in
  assert_bool "Diagonal placement should fail"
    (not (try place_ship grid 1 (0, 0) (4, 4) with _ -> false));
  assert_bool "Overlap placement should fail"
    (not (try place_ship grid 2 (0, 2) (0, 6) with _ -> false))

let test_shoot _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 4) in
  assert_equal "Hit!" (shoot grid (0, 0));
  assert_equal "Miss!" (shoot grid (1, 0));
  assert_equal "Already guessed this position!" (shoot grid (0, 0))

let test_ai_guess _ =
  let grid = create_grid 10 in
  let _ = place_ship grid 1 (0, 0) (0, 0) in
  let result = ai_guess grid in
  assert_bool "AI should hit or miss" (result = "Hit!" || result = "Miss!")

let test_check_game_over _ =
  let grid = create_grid 10 in
  assert_bool "New grid should not be game over" (not (check_game_over grid));
  ignore (place_ship grid 1 (0, 0) (0, 4));
  for i = 0 to 4 do
    ignore (shoot grid (0, i))
  done;
  assert_bool "All ships sunk should be game over" (check_game_over grid)

let test_invalid_input _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 1 (-1, 0) (0, -1)))

let test_char_to_index _ =
  assert_equal 0 (char_to_index 'A');
  assert_equal 0 (char_to_index 'a');
  assert_equal 25 (char_to_index 'z')

let test_validate_coordinates _ =
  assert_bool "Valid coordinates (5,5)" (validate_coordinates 5 5 10);
  assert_bool "Invalid coordinates (-1,5)"
    (not (validate_coordinates (-1) 5 10));
  assert_bool "Invalid coordinates (0,11)" (not (validate_coordinates 0 11 10))

let test_boundary_ship_placement _ =
  let grid = create_grid 10 in
  assert_bool "Place ship on the boundary should succeed"
    (place_ship grid 3 (9, 0) (9, 2));
  assert_bool "Place ship on the boundary should succeed"
    (place_ship grid 3 (0, 9) (2, 9))

let test_boundary_ship_placement_failure _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 4 (9, 8) (9, 11)))

let test_horizontal_ship_overlap _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 5 (2, 2) (2, 6));
  assert_bool "Overlap should fail"
    (not (try place_ship grid 6 (2, 5) (2, 9) with _ -> false))

let test_vertical_ship_overlap _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 7 (3, 3) (7, 3));
  assert_bool "Vertical overlap should fail"
    (not (try place_ship grid 8 (5, 3) (9, 3) with _ -> false))

let test_complete_ship_destruction _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 9 (1, 1) (1, 5));
  List.iter (fun i -> ignore (shoot grid (1, i))) [ 1; 2; 3; 4; 5 ];
  assert_equal "You sunk a ship!" (shoot grid (1, 5))

let test_multiple_shots_same_place _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 10 (4, 4) (4, 4));
  ignore (shoot grid (4, 4));
  assert_equal "Already guessed this position!" (shoot grid (4, 4))

let test_random_ai_guess_effectiveness _ =
  let grid = create_grid 10 in
  List.iter
    (fun i -> ignore (place_ship grid (20 + i) (i, i) (i, i + 4)))
    [ 0; 1; 2; 3; 4 ];
  let results = List.init 50 (fun _ -> ai_guess grid) in
  let hits = List.filter (fun x -> x = "Hit!") results in
  assert_bool "AI should hit at least once" (List.length hits > 0)

let test_ship_placement_in_empty_grid _ =
  let grid = create_grid 10 in
  assert_bool "Ship should place in empty grid"
    (place_ship grid 11 (0, 0) (0, 2))

let test_ship_placement_boundary _ =
  let grid = create_grid 10 in
  assert_bool "Ship should place on grid boundary"
    (place_ship grid 12 (9, 7) (9, 9))

let test_ship_placement_at_edges _ =
  let grid = create_grid 10 in
  assert_bool "Placement at top edge should succeed"
    (place_ship grid 13 (0, 0) (0, 3));
  assert_bool "Placement at bottom edge should succeed"
    (place_ship grid 14 (9, 6) (9, 9));
  assert_bool "Placement at left edge should succeed"
    (place_ship grid 15 (2, 0) (5, 0));
  assert_bool "Placement at right edge should succeed"
    (place_ship grid 16 (2, 9) (5, 9))

let test_ships_touching_at_corners _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 17 (0, 0) (0, 3));
  ignore (place_ship grid 18 (1, 4) (4, 4));
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 19 (0, 4) (0, 5)))

let test_ship_wrapping _ =
  let grid = create_grid 10 in
  assert_raises InvalidPlacement (fun () ->
      ignore (place_ship grid 20 (0, 8) (0, 11)))

let test_full_grid_sunk _ =
  let grid = create_grid 10 in
  for i = 0 to 9 do
    for j = 0 to 9 do
      ignore (place_ship grid ((i * 10) + j + 21) (i, j) (i, j))
    done
  done;
  List.iter
    (fun i ->
      List.iter
        (fun j -> ignore (shoot grid (i, j)))
        [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ])
    [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ];
  assert_bool "All cells sunk should be game over" (check_game_over grid)

let test_ai_efficiency_easy _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 220 (1, 1) (1, 4));
  set_ai_mode Easy;
  let results = List.init 50 (fun _ -> ai_guess grid) in
  let hits = List.filter (fun x -> x = "Hit!") results in
  assert_bool "Easy AI should have random efficiency"
    (List.length hits > 0 && List.length hits <= 50)

let test_ai_efficiency_hard _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 221 (5, 5) (5, 8));
  set_ai_mode Hard;
  let hits = ref 0 in
  for _ = 1 to 50 do
    if ai_guess grid = "Hit!" then incr hits
  done;
  assert_bool "Hard AI should be more efficient than Easy AI" (!hits > 10)

let powerup_grid = create_grid 10
let gold = ref 100
let bombed_rows = ref []
let bombed_columns = ref []
let bombed_squares = ref []

let bomb_row row =
  let final = ref "" in
  let y = char_to_index row in
  if List.mem y !bombed_rows then begin
    final := "Already bombed that row!"
  end
  else begin
    bombed_rows := y :: !bombed_rows;
    for i = 0 to Array.length powerup_grid - 1 do
      let result = shoot powerup_grid (y, i) in
      if result = "Hit!" then gold := !gold + 50;
      final := result
    done;
    gold := !gold - 100
  end;
  !final

let test_bomb_row _ =
  let _ = place_ship powerup_grid 1 (0, 1) (0, 5) in
  assert_bool "Bombing a fresh row" ("You sunk a ship!" = bomb_row 'A');
  assert_bool "Gold after bombing a ship length 5" (!gold = 250);
  assert_bool "Bombing previously bombed row"
    ("Already bombed that row!" = bomb_row 'A')

let powerup_grid2 = create_grid 10

let bomb_column col =
  let final = ref "" in
  let x = int_of_string col in
  if List.mem x !bombed_columns then begin
    final := "Already bombed that column!"
  end
  else begin
    bombed_columns := x :: !bombed_columns;
    for i = 0 to Array.length powerup_grid2 - 1 do
      let result = shoot powerup_grid2 (i, x) in
      if result = "Hit!" then gold := !gold + 50;
      final := result
    done;
    gold := !gold - 100
  end;
  !final

let test_bomb_column _ =
  let _ = place_ship powerup_grid2 1 (1, 0) (4, 0) in
  assert_bool "Bombing a fresh column" ("You sunk a ship!" = bomb_column "1");
  assert_bool "Gold after bombing a ship length 4" (!gold = 200);
  assert_bool "Bombing previously bombed column"
    ("Already bombed that column!" = bomb_column "1")

let powerup_grid3 = create_grid 10

let square_bomb row col =
  let final = ref "" in
  let y = char_to_index row in
  let x = int_of_string col in
  if not (validate_bomb y x (Array.length powerup_grid3)) then begin
    final := "Invalid selection!"
  end
  else if List.mem (y, x) !bombed_squares then begin
    final := "Already bombed that square!"
  end
  else begin
    bombed_squares := (y, x) :: !bombed_squares;
    for i = 0 to 2 do
      for j = 0 to 2 do
        let result = shoot powerup_grid3 (y + i, x + j) in
        if result = "Hit!" then gold := !gold + 50;
        final := result
      done
    done;
    gold := !gold - 100
  end;
  !final

let test_square_bomb _ =
  let _ = place_ship powerup_grid3 1 (1, 0) (4, 0) in
  assert_bool "Bombing a fresh square" ("You sunk a ship!" = square_bomb 'A' "1");
  assert_bool "Gold after bombing a ship length 1" (!gold = 50);
  assert_bool "Bombing previously bombed square"
    ("Already bombed that square!" = square_bomb 'A' "1")

let rec airstrike grid shots =
  let final = ref "" in
  let grid_size = Array.length grid in
  let x = Random.int grid_size and y = Random.int grid_size in
  if shots = 0 then print_string ""
  else
    let () = gold := !gold - 25 in
    match grid.(y).(x) with
    | Hit _ | Miss -> airstrike grid shots
    | _ ->
        let result = shoot grid (y, x) in
        if result = "Hit!" then
          let () = gold := !gold + 50 in
          let () = final := result in
          airstrike grid (shots - 1)
        else
          let () = final := result in
          airstrike grid (shots - 1)

let powerup_grid4 = create_grid 10

let test_airstrike _ =
  let _ = place_ship powerup_grid4 1 (1, 0) (4, 0) in
  let _ = airstrike powerup_grid4 4 in
  assert_bool "Commencing airstrike for 100 gold"
    (4 = count_hit_cells powerup_grid4 + count_cell_type powerup_grid4 Miss)

let suite =
  "Battleship Test Suite"
  >::: [
         "test_create_grid" >:: test_create_grid;
         "test_print_grid" >:: test_print_grid;
         "test_place_ship_success" >:: test_place_ship_success;
         "test_place_ship_failures" >:: test_place_ship_failures;
         "test_shoot" >:: test_shoot;
         "test_ai_guess" >:: test_ai_guess;
         "test_check_game_over" >:: test_check_game_over;
         "test_invalid_input" >:: test_invalid_input;
         "test_char_to_index" >:: test_char_to_index;
         "test_validate_coordinates" >:: test_validate_coordinates;
         "test_boundary_ship_placement" >:: test_boundary_ship_placement;
         "test_boundary_ship_placement_failure"
         >:: test_boundary_ship_placement_failure;
         "test_horizontal_ship_overlap" >:: test_horizontal_ship_overlap;
         "test_vertical_ship_overlap" >:: test_vertical_ship_overlap;
         "test_complete_ship_destruction" >:: test_complete_ship_destruction;
         "test_multiple_shots_same_place" >:: test_multiple_shots_same_place;
         "test_random_ai_guess_effectiveness"
         >:: test_random_ai_guess_effectiveness;
         "test_ship_placement_in_empty_grid"
         >:: test_ship_placement_in_empty_grid;
         "test_ship_placement_boundary" >:: test_ship_placement_boundary;
         "test_ship_placement_at_edges" >:: test_ship_placement_at_edges;
         "test_ships_touching_at_corners" >:: test_ships_touching_at_corners;
         "test_ship_wrapping" >:: test_ship_wrapping;
         "test_full_grid_sunk" >:: test_full_grid_sunk;
         "test_ai_efficiency_easy" >:: test_ai_efficiency_easy;
         "test_ai_efficiency_hard" >:: test_ai_efficiency_hard;
         "test_bomb_row" >:: test_bomb_row;
         "test_bomb_column" >:: test_bomb_column;
         "test_square_bomb" >:: test_square_bomb;
         "test_airstrike" >:: test_airstrike;
       ]

let () = run_test_tt_main suite
