type color = R | B | Emp

(**AF: the board is represented as an array of length 7, where each element is a
   color list. The head of each list represents the top disk in that row. If there
   is no disk in that row, the list is [Emp]
   RI: All of the lists representing columns have at most 6 elements and cannot
   be the empty list []
*)
type t = color list array

let empty = [|[Emp]; [Emp]; [Emp]; [Emp]; [Emp]; [Emp]; [Emp] |]

let make_move board column color = 
  (board.(column) <- 
     match board.(column) with
     |[] -> failwith "Representation Invariant Broken"
     |[Emp] -> [color]
     |h::t-> color::h::t); board

let get_as_list board = failwith "unimplemented"

let checkwin board = failwith "unimplemented"

let filled_slots board = failwith "unimplemented"

let is_full board = failwith "unimplemented"

let is_full_column board column = failwith "unimplemented"

let ascii_art board = failwith "unimplemented"
