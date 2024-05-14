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
    (try place_ship grid 1 (0, 0) (4, 4) with _ -> true);
  assert_bool "Overlap placement should fail"
    (try place_ship grid 2 (0, 2) (0, 6) with _ -> true)

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
  assert_bool "Overlap should fail" (not (place_ship grid 6 (2, 5) (2, 9)))

let test_vertical_ship_overlap _ =
  let grid = create_grid 10 in
  ignore (place_ship grid 7 (3, 3) (7, 3));
  assert_bool "Vertical overlap should fail"
    (not (place_ship grid 8 (5, 3) (9, 3)))

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
       ]

let () = run_test_tt_main suite
