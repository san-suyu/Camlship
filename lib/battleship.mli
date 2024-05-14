type cell =
  | Empty
  | Ship of int
  | Hit of int
  | Miss

type grid = cell array array

exception InvalidPlacement
exception InvalidInput

val create_grid : int -> grid
val print_grid : grid -> bool -> string -> unit
val place_ship : grid -> int -> int * int -> int * int -> bool
val shoot : grid -> int * int -> string
val ai_guess : grid -> string
val check_game_over : grid -> bool
val char_to_index : char -> int
val validate_coordinates : int -> int -> int -> bool
val validate_bomb : int -> int -> int -> bool
val airstrike : grid -> int -> unit
val random_place_ships : grid -> unit

type ai_mode =
  | Easy
  | Hard

val set_ai_mode : ai_mode -> unit
val get_ai_mode : unit -> ai_mode
