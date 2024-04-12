(* open Ship *)
type cell =
  | Empty
  | Ship

(* type cell = | Empty | Ship of { size : int; hit : bool; } *)

type grid = cell array array

(* type grid = { size : int; cells : cell array array; } *)

let create_grid size = Array.make_matrix size size Empty

(* let create_grid size = { size; cells = Array.make_matrix size size (Empty {
   hit = false }) } *)

let is_valid_placement (x1, y1) (x2, y2) size =
  (x1 = x2 && x1 <= size - 1) || (y1 = y2 && y1 <= size - 1)

(** Check if the cell is empty *)
let is_empty grid (y, x) = grid.(y).(x) = Empty

(** Check if user's target position is empty *)
let is_all_empty grid (x1, y1) (x2, y2) =
  let all_empty = ref true in
  for x = min x1 x2 to max x1 x2 do
    for y = min y1 y2 to max y1 y2 do
      if not (is_empty grid (y, x)) then all_empty := false
    done
  done;
  !all_empty

(* let place_ship grid (x1, y1) (x2, y2) size = if is_valid_placement (x1, y1)
   (x2, y2) size then begin for x = min x1 x2 to max x1 x2 do for y = min y1 y2
   to max y1 y2 do grid.(y).(x) <- Ship done done; true end else false *)

let place_ship grid (x1, y1) (x2, y2) size =
  if is_valid_placement (x1, y1) (x2, y2) size then
    if is_all_empty grid (x1, y1) (x2, y2) then begin
      for x = min x1 x2 to max x1 x2 do
        for y = min y1 y2 to max y1 y2 do
          grid.(y).(x) <- Ship
        done
      done;
      true
    end
    else begin
      Printf.printf "Error: Cannot place ship on occupied cells.\n";
      false
    end
  else begin
    Printf.printf
      "Error: Invalid placement. Ships must be placed in a straight line \
       within the grid bounds.\n";
    false
  end
