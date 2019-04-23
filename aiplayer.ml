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


(** Creates a new minmax tree based on the current board state, takes
    into account whether a column is full. Stops tree if column is full. *)
let generate_minmax_tree st = 
  (*gen_children *)
  let rec gen_children curr_state d =
    let rec loop_norm count = 
      if count = 7 then []
      else 
        match go count curr_state with
        |Legal res -> 
          if check_win (board res)=None then
            Node (-100, res, count, gen_children res (d+1))
            ::(loop_norm (count+1))
          else Node (100, res, count, [])::(loop_norm (count+1))
        |Illegal -> (loop_norm (count + 1)) in 
    let rec loop_leaf count = 
      if count = 7 then []
      else 
        match go count curr_state with
        |Legal res -> 
          if check_win (board res)=None then
            Node (Board.score (board res), res, count, gen_children res (d+1))
            ::(loop_norm (count+1))
          else Node (100, res, count, [])::(loop_norm (count+1))
        |Illegal -> (loop_norm (count + 1)) in 
    if d = depth then loop_leaf 0(*create the leaf layer *)
    else loop_norm 0 in
  (*loop over 7 Nodes *)
  (*call gen_Children on the root *)
  Node (-100, st, -1, gen_children st 0)

(** Takes fully evaluated tree with scores at Leafs (But not at nodes), 
    returns the next move for the ai to make *)

let eval_tree t = 

  let rec get_score children curr_score =
    match children with 
    |[]-> curr_score
    |Node (sc, st, m, c)::t -> 
      if State.current_player st = B then 
        max (get_score c sc) (get_score t curr_score)
      else 
        min (get_score c sc) (get_score t curr_score) in

  let scored_children root_children = List.map 
      (fun (Node (sc, st, m, c)) -> Node (get_score c sc, st, m, c)) 
      root_children in

  let extract_best_scoring_move scored_root_children =
    List.fold_left 
      (fun (best_sc, best_move) (Node (sc, st, move, c))->
         (if sc>best_sc then (sc, move)
          else (best_sc, best_move))
      ) (-100, -1) scored_root_children in

  let Node (sc, state, move, children) = t in

  extract_best_scoring_move (scored_children (children))


(** returns a state after the AI makes a move *)
let make_move_ai (st:State.t) : State.t = {
  board = Board.make_move st.board (eval_tree st) (st.current);
  current = st.current}