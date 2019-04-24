type color = R | B | Emp

(** AF: the board is represented as a 2d array of size 7x6 (columns x rows) where
    each array element represents a position in th board. Each array element is 
    of type color.
    RI: In a single column, there cannot be an Emp sandwiched between two colored
    disks. That is, the subarrays R; Emp; R, R; Emp B, B; Emp; R, B; Emp; B, are
    all invalid 
*)
type t = color array array

(**  *)
let empty = [|[|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|]; 
              [|Emp; Emp; Emp; Emp; Emp; Emp|];
              [|Emp; Emp; Emp; Emp; Emp; Emp|] |]

(**  *)
let board_copy board =
  let new_board = Array.make 7 (Array.make 6 Emp) in 
  (for x=0 to 6 do
     new_board.(x) <- (Array.copy (board.(x))) done); new_board

(** [make_move board column color] is the board resulting from dropping a disk
    of color [color] into the column numbered [column] where [column] is a number
    in the range [0..6] counting from the left. *)
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

(**  *)
let score_2x2 grid =
  let check_rows grid = 
    let rec loop r acc =
      if r = 2 then acc
      else if grid.(0).(r) = grid.(1).(r) && 
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
      if c = 2 then acc
      else if (grid.(c).(0) = grid.(c).(1)) && 
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
    if (grid.(0).(1) = grid.(1).(0)) && 
       grid.(0).(1) <> Emp
    then begin
      match grid.(0).(1) with
      | R -> -1
      | B -> 1
      | _ -> failwith "invalid"
    end 
    else if grid.(0).(0) = grid.(1).(1) && 
            grid.(0).(0) <> Emp
    then begin
      match grid.(0).(0) with
      | R -> -1
      | B -> 1
      | _ -> failwith "invalid"
    end 
    else 0 in
  check_rows grid + check_cols grid + check_diags grid

let score_1x2 grid = 
  if (grid.(0).(0) = grid.(0).(1)) && 
     grid.(0).(0) <> Emp
  then begin
    match grid.(0).(0) with
    | R -> -1
    | B -> 1
    | _ -> failwith "invalid"
  end 
  else 0

(** [check_win board] is [Some c] if the color [c] has won the game 
    represented by [board], or [None] if no one has won the game. *)
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
      if c = 4 then None
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
  let rec loop colmarker rowmarker = 
    if colmarker = 3 then None
    else if rowmarker = 4 then loop (colmarker + 1) 0
    else begin
      let subgrid = sub_array_2d board rowmarker 4 colmarker 4 in
      match (check_diags subgrid, check_cols subgrid, check_rows subgrid) with
      | (None, None, None) -> loop colmarker (rowmarker + 1)
      | (None, None, c) | (None, c, _) | (c, _, _) -> c
    end in
  loop 0 0

let score board =
  let sub_array_2d arr x_start x_len y_start y_len = 
    let x_sub = Array.sub arr x_start x_len in
    ((for x = 0 to x_len - 1
      do x_sub.(x) <- Array.sub x_sub.(x) y_start y_len done); x_sub) in
  let rec loop colmarker rowmarker acc = 
    if colmarker > 4 then acc
    else if rowmarker > 6 then loop (colmarker + 2) 0 acc
    else begin
      if (rowmarker <> 6) then begin
        let subgrid = sub_array_2d board rowmarker 2 colmarker 2 in
        loop colmarker (rowmarker + 2) (acc + score_2x2 subgrid)
      end
      else begin 
        let subgrid = sub_array_2d board rowmarker 1 colmarker 2 in
        loop colmarker (rowmarker + 1) (acc + score_1x2 subgrid)
      end
    end in 
  let win_score board = 
    match check_win board with 
    | Some c -> if c = R then -50 else 50
    | None -> 0 in
  loop 0 0 0 + win_score board

(**  *)
let filled_slots_helper column  = 
  let rec loop counter =
    if counter = 6 then 0
    else if column.(counter) <> Emp then 1 + loop (counter + 1)
    else loop (counter + 1) in 
  loop 0

(** [filled_slots board] is the number of slots (positions) in the board
    that have a disk in it. This is equal to the number of moves made since the 
    beginning of the game. *)
let filled_slots board = 
  Array.fold_left (fun acc col -> (filled_slots_helper col) + acc) 0 board

(** [is_full board] is [true] if all the slots (positions) in [board] have
    disks in them. That is, no more moves can be made. *)
let is_full board = filled_slots board = 42

(** [is_full_column board c] is [true] if no more disks can be dropped into 
    column [c]. *)
let is_full_column board column = not (Array.mem Emp (board.(column)))

(** [ascii_art board] is a string containing an ASCII art representation
    of [board] that can be printed to the terminal during gameplay. *)
let ascii_art board =
  let rec loop r c = 
    if r = -1 then "\n"
    else if c = 7 then "\n" ^ loop (r - 1) 0
    else begin
      match board.(c).(r) with 
      | R -> "\027[31mO\027[0m  " ^ (loop r (c + 1))
      | B -> "\027[34mO\027[0m  " ^ (loop r (c + 1))
      | Emp -> "O  " ^ (loop r (c + 1)) 
    end in
  loop 5 0

(** [color_string color] is the color in string format. *)
let color_string color =
  match color with
  | R -> "Red"
  | B -> "Blue"
  | Emp -> "Emp"


