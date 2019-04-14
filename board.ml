type color = R | B | Emp

type t = (color list) list

let empty =
  let col = 
    let rec loop_col acc =
      if acc = 6 then []
      else Emp::(loop_col (acc+1)) in
    loop_col 0 in 

  let rec loop_row acc =
    if acc = 7 then []
    else col::(loop_row (acc+1)) in
  loop_row 0