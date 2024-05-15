(* custom ship structure with detailed properties of each ship *)
type custom_ship = {
  (* unique identifier for a ship *)
  id : int;
  (* list of coordinates occupied by ship *)
  cells : (int * int) list;
  (* health of the ship, typically equal to number of cells it occupies on
     grid *)
  health : int;
  (* top-left coordinate of the ship boundary*)
  top_left : int * int;
  (* width of the ships boundary *)
  width : int; (* height of ships boundary *)
  height : int;
}

(* Abstraction function: A custom_ship represents a unique ship on the grid with
   [id] as its identifier. The ship occupies [cells], each corresponding to a
   coordinate on the grid. [health] indicates the number of unhit cells, with
   initial value equal to the length of [cells]. [top_left] is the smallest
   coordinate by value that defines the upper left corner of the ship's bounding
   box. [width] and [height] define the dimensions of the smallest rectangle
   that can enclose the entire ship.

   Representation invariant: - [health] must be non-negative. - All coordinates
   in [cells] must be unique and within the grid bounds determined by the grid
   size. - [width] and [height] should be correctly calculated to contain all
   coordinates in [cells]. - [top_left] must actually be the coordinate with the
   smallest x and y values in [cells]. *)

(* types of cells that can be present on the game grid *)
type cell =
  (* empty cell with no ships or mines *)
  | Empty
  (* cell containing part of a ship identified by an int *)
  | Ship of int
  (* cell containing a mine *)
  | Mine
  (*cell that was hit containing part of a ship identified by an int *)
  | Hit of int
  (* cell where a mine has exploded *)
  | Exploded
  (*cell that was shot at but was empty *)
  | Miss
  (* cell occupied by a custom ship *)
  | CustomShip of custom_ship
  (* cell containing a hit part of a custom ship *)
  | HitCustom of custom_ship

(* type representing the game grid as a 2D array of cells *)
type grid = cell array array

(* exceptions that can be raised during the game *)
(* raised when a ship or mine cannot be placed at the specified location *)
exception InvalidPlacement

(* raised when an input, such as coordinates, are invalid *)
exception InvalidInput

(* creates grid of specified size initialized with Empty cells *)
val create_grid : int -> grid

(* prints current state of the grid to stdout *)
val print_grid : grid -> bool -> string -> unit

(* tries to place a ship at specified coordinates, returns true if successful *)
val place_ship : grid -> int -> int * int -> int * int -> bool

(* tries to place a mine at specified coordinates, returns true if successful *)
val place_mine : grid -> int * int -> bool

(* processes a shot at the specified coordinates, returning result message *)
val shoot : grid -> int * int -> string

(* handles explosion of a mine, returning a result message *)
val mine_shot : grid -> string

(*makes a guess for the AI based on the set AI mode, returning result message *)
val ai_guess : grid -> string

(* checks if game is over (all ships have been sunk) *)
val check_game_over : grid -> bool

(* converts a character to the corresponding index (e.g., 'A' to 0) *)
val char_to_index : char -> int

(* validates if specified coordinates are within the grid boundaries *)
val validate_coordinates : int -> int -> int -> bool

(* valides if a bomb placement is within the grid boundaries *)
val validate_bomb : int -> int -> int -> bool

(* randomly places ships on grid *)
val random_place_ships : grid -> unit

(* randomly places a specified number of mines on grid *)
val random_place_mines : grid -> int -> unit

(* AI difficulty modes *)
type ai_mode =
  (* easy difficulty setting for AI *)
  | Easy
  (* hard difficulty setting for AI *)
  | Hard

(* sets current AI difficulty mode *)
val set_ai_mode : ai_mode -> unit

(* retrieves current AI difficulty mode *)
val get_ai_mode : unit -> ai_mode

(* counts cells of a specified type on grid *)
val count_cell_type : grid -> cell -> int

(* counts number of hit cells on grid *)
val count_hit_cells : grid -> int

(* assembles a custom ship from a list of coordinates *)
val assemble_custom_ship : (int * int) list list -> int -> custom_ship

(* attempts to place a custom ship on the grid at specified coordinates, returns
   true if successful *)
val place_custom_ship : grid -> custom_ship -> int * int -> bool

(* returns the health and length of all ships combined *)
val get_ship_health_length : unit -> int

(* calculates bounding box for a list of coordinates *)
val get_bounding_box : (int * int) list -> (int * int) * int * int

(* creates a custom ship from existing cells on the grid *)
val create_custom_ship_from_grid : grid -> custom_ship

(* prints details of a custom ship *)
val print_custom_ship : custom_ship -> unit
