type custom_ship = {
  id : int;
  cells : (int * int) list;
  health : int;
  top_left : int * int;
  width : int;
  height : int;
}

type cell =
  | Empty
  | Ship of int
  | Mine
  | Hit of int
  | Exploded
  | Miss
  | CustomShip of custom_ship
  | HitCustom of custom_ship

type grid = cell array array

exception InvalidPlacement
exception InvalidInput

val create_grid : int -> grid
val print_grid : grid -> bool -> string -> unit
val place_ship : grid -> int -> int * int -> int * int -> bool
val place_mine : grid -> int * int -> bool
val shoot : grid -> int * int -> string
val mine_shot : grid -> string
val ai_guess : grid -> string
val check_game_over : grid -> bool
val char_to_index : char -> int
val validate_coordinates : int -> int -> int -> bool
val validate_bomb : int -> int -> int -> bool
val random_place_ships : grid -> unit
val random_place_mines : grid -> int -> unit

type ai_mode =
  | Easy
  | Hard

val set_ai_mode : ai_mode -> unit
val get_ai_mode : unit -> ai_mode
val count_cell_type : grid -> cell -> int
val count_hit_cells : grid -> int
val assemble_custom_ship : (int * int) list list -> int -> custom_ship
val place_custom_ship : grid -> custom_ship -> int * int -> bool
val get_ship_health_length : unit -> int
val get_bounding_box : (int * int) list -> (int * int) * int * int
val create_custom_ship_from_grid : grid -> custom_ship
val print_custom_ship : custom_ship -> unit
