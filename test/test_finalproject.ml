open OUnit2
(* open Battleship *)

let valid_ship_placement_tests =
  "test suite for valid ship placements"
  >::: [
         ( "within grid" >:: fun _ ->
           assert_equal true
             (Battleship.is_valid_placement (1, 1) (2, 1) 5)
             ~printer:string_of_bool );
         ( "outside of grid" >:: fun _ ->
           assert_equal false
             (Battleship.is_valid_placement (1, 1) (2, 1) 0)
             ~printer:string_of_bool );
         ( "edge of grid" >:: fun _ ->
           assert_equal true
             (Battleship.is_valid_placement (2, 2) (2, 2) 2)
             ~printer:string_of_bool );
       ]

let _ = run_test_tt_main valid_ship_placement_tests
