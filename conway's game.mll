let create_grid size = Array.make_matrix ~dimx:size ~dimy:size false

let count_neighbors grid x y =
  let size = Array.length grid in
  let delta = [-1; 0; 1] in
  List.fold_left (fun acc dx ->
    acc + List.fold_left (fun acc dy ->
      if dx = 0 && dy = 0 then acc
      else
        let nx = x + dx in
        let ny = y + dy in
        if nx >= 0 && nx < size && ny >= 0 && ny < size then
          if grid.(nx).(ny) then acc + 1 else acc
        else acc
    ) acc delta
  ) 0 delta

let update_grid grid =
  let size = Array.length grid in
  let new_grid = Array.copy grid in
  for x = 0 to size - 1 do
    for y = 0 to size - 1 do
      let neighbors = count_neighbors grid x y in
      new_grid.(x).(y) <- match grid.(x).(y) with
        | true -> neighbors = 2 || neighbors = 3
        | false -> neighbors = 3
    done
  done;
  new_grid

let print_grid grid =
  Array.iter (fun row ->
    Array.iter (fun cell ->
      print_string (if cell then "#" else ".")
    ) row;
    print_endline ""
  ) grid;
  print_endline ""

let () =
  let size = 10 in
  let grid = create_grid size in
  (* Initialize grid with some pattern *)
  grid.(1).(2) <- true;
  grid.(2).(3) <- true;
  grid.(3).(1) <- true;
  grid.(3).(2) <- true;
  grid.(3).(3) <- true;
  print_endline "Initial grid:";
  print_grid grid;
  let new_grid = update_grid grid in
  print_endline "Next generation:";
  print_grid new_grid
