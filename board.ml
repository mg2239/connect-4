type color = R | B | Emp

(** AF: the board is represented as a 2d array of size 7x6 (columns x rows) where
    each array element represents a position in th board. Each array element is 
    of type color.
    RI: In a single column, there cannot be an Emp sandwiched between two colored
    disks. That is, the subarrays R; Emp; R, R; Emp B, B; Emp; R, B; Emp; B, are
    all invalid 
*)
type t = color array array

let empty = [|[|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|];
              [|Emp; Emp; Emp; Emp; Emp; Emp|] |]

let ascii_art board =
  let rec looprows r = 
    if r = -1 then "\n"
    else
      let rec loopcols c = 
        if c = 7 then "\n"
        else 
          match board.(c).(r) with 
          | R -> "\027[31mO\027[0m  " ^ (loopcols (c + 1))
          | B -> "\027[34mO\027[0m  " ^ (loopcols (c + 1))
          | Emp -> "O  " ^ (loopcols (c + 1)) in
      (loopcols 0) ^ (looprows (r - 1)) in
  looprows 5

let board_copy board =
  let new_board = Array.make 7 (Array.make 6 Emp) in 
  (for x=0 to 6 do
     new_board.(x) <- (Array.copy (board.(x))) done); new_board

let make_move board column color = 
  let new_board = board_copy board in
  let find_top col = 
    let rec loop count =
      if col.(count) = Emp then count 
      else if count = 7 then failwith "Invalid Move"
      else loop (count+1) in
    loop 0 in
  (new_board.(column).(find_top (new_board.(column)))<-color);  new_board

let get_as_list (board: color array array) = 
  let rec loop acc =
    if acc = 7 then []
    else Array.to_list(board.(acc))::(loop (acc+1)) in
  loop 0

let score board =
  let check_rows grid = 
    let rec loop r acc =
      if r = 3 then acc
      else if grid.(0).(r) = grid.(1).(r) && 
              grid.(1).(r) = grid.(2).(r) && 
              grid.(0).(r) <> Emp
      then begin
        match grid.(0).(r) with
        | R -> loop (r + 1) (acc - 1)
        | B -> loop (r + 1) (acc + 1)
        | _ -> failwith "invalid"
      end 
      else loop (r + 1) acc in 
    loop 0 0 in
  let check_cols grid = 
    let rec loop c acc = 
      if c = 3 then acc
      else if (grid.(c).(0) = grid.(c).(1)) && 
              (grid.(c).(1) = grid.(c).(2)) && 
              grid.(c).(0) <> Emp
      then begin
        match grid.(c).(0) with
        | R -> loop (c + 1) (acc - 1)
        | B -> loop (c + 1) (acc + 1)
        | _ -> failwith "invalid"
      end 
      else loop (c + 1) acc in 
    loop 0 0 in
  let check_diags grid = 
    if (grid.(0).(2) = grid.(1).(1)) && 
       (grid.(1).(1) = grid.(2).(0)) && 
       grid.(0).(2) <> Emp
    then begin
      match grid.(0).(2) with
      | R -> -1
      | B -> 1
      | _ -> failwith "invalid"
    end 
    else if grid.(0).(0) = grid.(1).(1) && 
            grid.(1).(1) = grid.(2).(2) && 
            grid.(0).(0) <> Emp
    then begin
      match grid.(0).(0) with
      | R -> -1
      | B -> 1
      | _ -> failwith "invalid"
    end 
    else 0 in
  let sub_array_2d arr x_start x_len y_start y_len = 
    let x_sub = Array.sub arr x_start x_len in
    ((for x = 0 to x_len - 1
      do x_sub.(x) <- Array.sub x_sub.(x) y_start y_len done); x_sub) in
  let check_subgrids = 
    let rec loopcols colmarker acc = 
      if colmarker = 4 then acc
      else
        let rec looprows rowmarker acc = 
          if rowmarker = 5 then acc
          else
            let subgrid = sub_array_2d board rowmarker 3 colmarker 3 in
            looprows (rowmarker + 1) (acc + check_cols subgrid + check_diags subgrid + check_rows subgrid) in
        loopcols (colmarker + 1) (looprows 0 acc) in
    loopcols 0 0
  in
  check_subgrids

let check_win (board:color array array) =
  let check_rows grid = 
    let rec loop r =
      if r = 4 then None
      else if grid.(0).(r) = grid.(1).(r) && 
              grid.(1).(r) = grid.(2).(r) && 
              grid.(2).(r) = grid.(3).(r) && 
              grid.(0).(r) <> Emp 
      then Some (grid.(0).(r))
      else loop (r + 1) in
    loop 0 in 
  let check_cols grid = 
    let rec loop c = 
      if c=4 then None
      else if (grid.(c).(0) = grid.(c).(1)) && 
              (grid.(c).(1) = grid.(c).(2)) && 
              (grid.(c).(2) = grid.(c).(3)) && 
              grid.(c).(0) <> Emp
      then Some (grid.(c).(0))
      else loop (c + 1) in 
    loop 0 in
  let check_diags grid = 
    if (grid.(0).(3) = grid.(1).(2)) && 
       (grid.(1).(2) = grid.(2).(1)) && 
       (grid.(2).(1) = grid.(3).(0)) && 
       grid.(0).(3) <> Emp
    then Some (grid.(0).(3))
    else if grid.(0).(0) = grid.(1).(1) && 
            grid.(1).(1) = grid.(2).(2) && 
            grid.(2).(2) = grid.(3).(3) && 
            grid.(0).(0) <> Emp
    then Some (grid.(0).(0))
    else None
  in 
  let sub_array_2d (arr: t) x_start x_len y_start y_len = 
    let x_sub = Array.sub arr x_start x_len in
    ((for x = 0 to x_len - 1
      do x_sub.(x) <- Array.sub x_sub.(x) y_start y_len done); x_sub) in
  let check_subgrids = 
    (*loop through columns (0-3) *)
    let rec loopcols colmarker = 
      if colmarker = 3 then None
      else
        let rec looprows rowmarker = 
          if rowmarker = 4 then None
          else
            let subgrid = sub_array_2d board rowmarker 4 colmarker 4 in
            match check_diags subgrid with
            | None -> 
              (match check_cols subgrid with
               | None -> (match check_rows subgrid with
                   | None -> looprows (rowmarker + 1)
                   | x -> x)
               | y -> y)
            | z -> z in
        match looprows 0 with 
        | None -> loopcols (colmarker + 1)
        | x -> x in
    loopcols 0 
  in
  check_subgrids

let filled_slots_helper column  = 
  let rec loop counter =
    if counter = 6 then 0
    else if column.(counter) <> Emp then 1 + loop (counter+1)
    else loop (counter + 1) in 
  loop 0

let filled_slots board = 
  Array.fold_left (fun acc col -> (filled_slots_helper col) + acc) 0 board

let is_full board = filled_slots board = 42

let is_full_column board column = not (Array.mem Emp (board.(column)))

let ascii_art board =
  let rec looprows r = 
    if r = -1 then "\n"
    else
      let rec loopcols c = 
        if c = 7 then "\n"
        else 
          match board.(c).(r) with 
          | R -> "\027[31mO\027[0m  " ^ (loopcols (c + 1))
          | B -> "\027[34mO\027[0m  " ^ (loopcols (c + 1))
          | Emp -> "O  " ^ (loopcols (c + 1)) in
      (loopcols 0) ^ (looprows (r - 1)) in
  looprows 5

let color_string color =
  match color with
  | R -> "Red"
  | B -> "Blue"
  | Emp -> "Emp"


