type cell =
  | Empty
  | Ship
  | Hit
  | Miss

type grid = cell array array

let create_grid size = Array.make_matrix size size Empty
let char_to_index c = Char.code (Char.uppercase_ascii c) - Char.code 'A'

let is_valid_placement (y1, x1) (y2, x2) =
  (x1 = x2 && abs (y2 - y1) > 0) || (y1 = y2 && abs (x2 - x1) > 0)

(* Define a type to hold ship details *)
type ship_detail = {
  start : int * int;
  end_ : int * int;
  length : int;
}

(* List to track details of each ship *)
let ship_details : ship_detail list ref = ref []

(* Modified place_ship function to track ship placements *)
let place_ship grid (y1, x1) (y2, x2) =
  if is_valid_placement (y1, x1) (y2, x2) then
    let dir = if y1 = y2 then `Horizontal else `Vertical in
    let length = max (abs (y2 - y1)) (abs (x2 - x1)) + 1 in
    let coords =
      List.init length (fun i ->
          match dir with
          | `Horizontal -> if x1 < x2 then (y1, x1 + i) else (y2, x2 + i)
          | `Vertical -> if y1 < y2 then (y1 + i, x1) else (y2 + i, x2))
    in
    if List.for_all (fun (y, x) -> grid.(y).(x) = Empty) coords then begin
      List.iter (fun (y, x) -> grid.(y).(x) <- Ship) coords;
      (* Track each ship placement *)
      ship_details :=
        { start = (y1, x1); end_ = (y2, x2); length } :: !ship_details;
      true
    end
    else false
  else false

let random_placement grid num_ships max_ship_size =
  Random.self_init ();
  let rec place_random_ship n =
    if n > 0 then
      let x = Random.int 10
      and y = Random.int 10
      and horizontal = Random.bool ()
      and size = Random.int max_ship_size + 1 in
      let x2 = if horizontal then x + size - 1 else x
      and y2 = if horizontal then y else y + size - 1 in
      if x2 < 10 && y2 < 10 && place_ship grid (y, x) (y2, x2) then
        place_random_ship (n - 1)
      else place_random_ship n
  in
  place_random_ship num_ships

(* converts grid to arraylist*)
let grid_to_list grid = Array.to_list (Array.map Array.to_list grid)

(* retrieves detailed information about each ship placed *)
let get_ships_info () = !ship_details
