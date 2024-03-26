type cell =
  | Empty
  | Ship

type grid = cell array array

let create_grid size = Array.make_matrix size size Empty
let is_valid_placement (x1, y1) (x2, y2) = x1 = x2 || y1 = y2

let place_ship grid (x1, y1) (x2, y2) =
  if is_valid_placement (x1, y1) (x2, y2) then begin
    for x = min x1 x2 to max x1 x2 do
      for y = min y1 y2 to max y1 y2 do
        grid.(y).(x) <- Ship
      done
    done;
    true
  end
  else false
