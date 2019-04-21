open Board
open State

let depth = 3

(**The minmax tree of possible moves is represented as a 7-ary tree where
   the root node is the current state and the children of a node are the states
   resulting from possible moves. A leaf in the tree is represented by a node
   whose list of child nodes is empty. Scores are initially only evaluated at
   a leaf. Score values at all other nodes are set to 0.
   A Node has attributes score:int, state: State.t, next_move:int, 
   children:minmaxtree list (of maximum size 7)
*)
type minmaxtree = Node of int*State.t*int*(minmaxtree list)



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


(** Creates a new minmax tree based on the current board state, takes
    into account whether a column is full. Stops tree if column is full. *)
let generate_minmax_tree st = 
  failwith ""

(** Takes fully evaluated tree with scores at Leafs (But not at nodes), 
    returns the next move for the ai to make *)
let eval_tree t = failwith ""

(** returns a state after the AI makes a move *)
let make_move_ai st = failwith ""