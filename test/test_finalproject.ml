open OUnit2
open Battleship

let test_grid1 = Battleship.create_grid 10

let valid_ship_placement_tests =
  "test suite for valid ship placements"
  >::: [
         ( "within grid" >:: fun _ ->
           assert_equal true
             (is_valid_placement test_grid1 (2, 2) (4, 2))
             ~printer:string_of_bool );
         ( "outside of grid" >:: fun _ ->
           assert_equal false
             (is_valid_placement test_grid1 (1, 1) (11, 1))
             ~printer:string_of_bool );
         ( "edge of grid" >:: fun _ ->
           assert_equal true
             (is_valid_placement test_grid1 (1, 1) (10, 1))
             ~printer:string_of_bool );
         ( "not straight ship" >:: fun _ ->
           assert_equal false
             (is_valid_placement test_grid1 (1, 1) (2, 2))
             ~printer:string_of_bool );
         ( "no ships in coordinates" >:: fun _ ->
           assert_equal true
             (place_ship test_grid1 (1, 1) (2, 1))
             ~printer:string_of_bool );
         ( "ships in coordinates; valid check" >:: fun _ ->
           assert_equal false
             (is_valid_placement test_grid1 (1, 1) (1, 2))
             ~printer:string_of_bool );
         ( "ships in coordinates" >:: fun _ ->
           assert_equal false
             (place_ship test_grid1 (1, 1) (1, 2))
             ~printer:string_of_bool );
         ( "ships in coordinates, but out of order coords" >:: fun _ ->
           assert_equal false
             (place_ship test_grid1 (1, 2) (1, 1))
             ~printer:string_of_bool );
       ]

let empty_grid = Battleship.create_grid 10
let test_grid2 = Battleship.create_grid 10
let () = random_placement test_grid2 5 4

let random_ship_placement_tests =
  "test suite for random ship placements"
  >::: [
         ( "ships in given grid" >:: fun _ ->
           assert_equal true (ships_in_grid test_grid2) ~printer:string_of_bool
         );
         ( "no ships in empty grid" >:: fun _ ->
           assert_equal false (ships_in_grid empty_grid) ~printer:string_of_bool
         );
         ( "random placement of 5 ships" >:: fun _ ->
           assert_equal 5 (List.length !ship_details) ~printer:string_of_int );
       ]

let _ = run_test_tt_main valid_ship_placement_tests
let _ = run_test_tt_main random_ship_placement_tests
