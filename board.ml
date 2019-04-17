type color = R | B | Emp

(**AF: the board is represented as a 2d array of size 7x6 (columns x rows) where
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


let make_move board column color = 
  let find_top col = 
    let rec loop count =
      if col.(count) = Emp then count 
      else if count = 7 then failwith "Invalid Move"
      else loop (count+1) in
    loop 0 in
  (board.(column).(find_top board.(column))<-color); board

let get_as_list board = 
  let rec loop acc =
    if acc = 7 then []
    else Array.to_list(board.(acc))::(loop (acc+1)) in
  loop 0

let check_win board =
  let check_rows grid = 
    let rec loop r =
      if r=4 then None
      else if grid.(0).(r)=grid.(1).(r) && 
              grid.(1).(r)=grid.(2).(r) && 
              grid.(2).(r)=grid.(3).(r) && 
              grid.(0).(r) <> Emp && 
              grid.(1).(r) <> Emp &&
              grid.(2).(r) <> Emp &&
              grid.(3).(r) <> Emp then Some (grid.(0).(r))
      else loop (r+1) in
    loop 0 in 
  let check_cols grid = 
    let rec loop c = 
      if c=4 then None
      else if grid.(c).(0)=grid.(c).(1) && 
              grid.(c).(1)=grid.(c).(2) && 
              grid.(c).(2)=grid.(c).(3) && 
              grid.(c).(0) <> Emp && 
              grid.(c).(1) <> Emp &&
              grid.(c).(2) <> Emp &&
              grid.(c).(3) <> Emp then Some (grid.(c).(0))
      else loop (c+1) in 
    loop 0 in
  let check_diags grid = 
    if grid.(0).(3)=grid.(1).(2) && 
       grid.(1).(2)=grid.(2).(1) && 
       grid.(2).(1)=grid.(3).(3) &&
       grid.(0).(3) <> Emp && 
       grid.(1).(2) <> Emp &&
       grid.(2).(1) <> Emp &&
       grid.(3).(3) <> Emp then Some (grid.(0).(3))
    else if grid.(0).(0)=grid.(1).(1) && 
            grid.(1).(1)=grid.(2).(2) && 
            grid.(2).(2)=grid.(3).(3) &&
            grid.(0).(0) <> Emp && 
            grid.(1).(1) <> Emp &&
            grid.(2).(2) <> Emp &&
            grid.(3).(3) <> Emp then Some (grid.(0).(0))
    else None
  in 
  let check_subgrids = 
    (*loop through columns (0-3) *)
    let rec loopcols colmarker = 
      if colmarker = 4 then None
      else
        let rec looprows rowmarker = 
          if rowmarker = 3 then None
          else
            let subgrid = Array.sub (Array.sub board rowmarker 4) colmarker 4 in
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
  in check_subgrids

let filled_slots_helper column  = 
  let rec loop counter =
    if counter = 6 then 0
    else if column.(counter) <> Emp then 1 + loop (counter+1)
    else loop (counter+1) in 
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
          | R -> "R  "^(loopcols (c+1))
          | B -> "B  "^(loopcols (c+1))
          | Emp -> "O  "^(loopcols (c+1)) in
      (loopcols 0)^(looprows (r-1)) in
  looprows 5

let color_string color =
  match color with
  | R -> "Red"
  | B -> "Black"
  | Emp -> "Empty"
