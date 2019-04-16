type color = R | B | Emp

(**AF: the board is represented as an array of length 7, where each element is a
   color list. The head of each list represents the top disk in that row. If there
   is no disk in that row, the list is the empty list[]
   RI: All of the lists representing columns have at most 6 elements
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
    else board.(acc)::(loop (acc+1)) in
  loop 0

let checkwin board =

  let check_rows grid = 
    let rec loop r =
      if r=4 then None
      else if grid.(0).(r)=grid.(1).(r) && grid.(1).(r)=grid.(2).(r)
              && grid.(2).(r)=grid.(3).(r) then Some (grid.(0).(r))
      else loop (r+1) in
    loop 0 in 

  let check_cols grid = 
    let rec loop c = 
      if c=4 then None
      else if grid.(c).(0)=grid.(c).(1) && grid.(c).(1)=grid.(c).(2) 
              && grid.(c).(2)=grid.(c).(3) then Some (grid.(c).(0))
      else loop (c+1) in 
    loop 0 in

  let check_diags grid = 
    if grid.(0).(3)=grid.(1).(2) && grid.(1).(2)=grid.(2).(1)
       && grid.(2).(1)=grid.(3).(3) then Some (grid.(0).(3))
    else if grid.(0).(0)=grid.(1).(1) && grid.(1).(1)=grid.(2).(2) 
            && grid.(2).(2)=grid.(3).(3) then Some (grid.(0).(0))
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
            |None -> 
              (match check_cols subgrid with
               |None -> (match check_rows subgrid with
                   |None -> looprows (rowmarker + 1)
                   |x -> x)
               |y -> y)
            |z -> z in
        match looprows 0 with 
        |None -> loopcols (colmarker + 1)
        |x -> x in
    loopcols 0 
  in
  check_subgrids

let filled_slots board = 
  let rec loop count =
    if count = 7 then 0
    else Array.length (board.(count)) + loop (count + 1) in
  loop 0

let is_full board = filled_slots board = 42

let is_full_column board column = Array.length (board.(column)) = 6

let ascii_art board =
  let rec looprows r = 
    if r = -1 then "\n"
    else
      let rec loopcols c = 
        if c = 7 then "\n"
        else 
          match board.(c).(r) with 
          |R -> "R  "^(loopcols (c+1))
          |B -> "B  "^(loopcols (c+1))
          |Emp -> "O  "^(loopcols (c+1)) in
      loopcols 0 in
  looprows 5



