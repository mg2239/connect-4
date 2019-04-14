type color = R | B 

(**AF: the board is represented as an array of length 7, where each element is a
   color list. The head of each list represents the top disk in that row. If there
   is no disk in that row, the list is the empty list[]
   RI: All of the lists representing columns have at most 6 elements
*)
type t = color list array

let empty = [|[]; []; []; []; []; []; [] |]

let make_move board column color = 
  (board.(column) <- 
     match board.(column) with
     |[] -> [color]
     |h::t-> color::h::t); board

let get_as_list board = 
  let rec loop acc =
    if acc = 7 then []
    else board.(acc)::(loop (acc+1)) in
  loop 0

let checkwin board = failwith "unimplemented"

let filled_slots board = 
  let rec loop count =
    if count = 7 then 0
    else List.length (board.(count)) + loop (count + 1) in
  loop 0

let is_full board = filled_slots board = 42

let is_full_column board column = List.length (board.(column))

let ascii_art board = failwith "unimplemented"
