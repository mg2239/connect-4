open Board
open State

(** The minmax tree of possible moves is represented as a 7-ary tree where
    the root node is the current state and the children of a node are the states
    resulting from possible moves. A leaf in the tree is represented by a node
    whose list of child nodes is empty. Scores are initially only evaluated at
    a leaf. Score values at all other nodes are set to 0.
    A Node has attributes score:int, state: State.t, next_move:int, 
    children:minmaxtree list (of maximum size 7)
*)
type minmaxtree = Node of int * State.t * int * minmaxtree list

(** [generate_minmax_tree st] is a minmax tree based on the current state [st].
    Takes into account whether a column is full, and stops tree if column is 
    full. *)
let generate_minmax_tree st depth = 
  (*gen_children *)
  let rec gen_children curr_state d =
    (*recursive loop to create inner nodes in minmax tree *)
    let rec loop_norm count = 
      if count = 7 then []
      else 
        match go count curr_state with
        |Legal res -> 
          let check_win_res = check_win (board res) in
          if check_win_res=None then
            Node (-500, res, count, gen_children res (d+1))
            ::(loop_norm (count+1))
          else if check_win_res=(Some (B))
          then Node (100, res, count, [])::(loop_norm (count+1))
          else Node (-500, res, count, [])::(loop_norm (count+1))
        |Illegal -> (loop_norm (count + 1)) in 
    (*recursive loop to create the leaf layer in the minmax tree *)
    let rec loop_leaf count = 
      if count = 7 then []
      else 
        match go count curr_state with
        |Legal res -> 
          let check_win_res = check_win (board res) in
          if check_win_res=None then
            Node (Board.score (board res), res, count, [])
            ::(loop_leaf (count+1))
          else if check_win_res=(Some (B)) 
          then Node (100, res, count, [])::(loop_leaf (count+1))
          else Node (-500, res, count, [])::(loop_leaf (count+1))
        |Illegal -> (loop_leaf (count + 1)) in 
    if d = depth then loop_leaf 0 (*create the leaf layer *)
    else loop_norm 0 in
  (* loop over 7 Nodes *)
  (* call gen_Children on the root *)
  Node (-100, st, -1, gen_children st 0)

(** [eval_tree t] is the column number that the AI should play in, determined by
    evaluating a minmax tree [t]. *)
let eval_tree t = 

  (**[get score children curr_score] returns the best score for the AI by 
     comparing all the leaves in the min max tree. [children] is a minmaxtree list
     representing a layer in the tree. At each node, the minimum or maximum score
     is selected depending on whether the current player is Red or Blue. This
     is where the minmax algorithm is implemented.*)
  let rec get_score children curr_score =
    match children with 
    | []-> curr_score (*reached leaf*)
    | Node (sc, st, m, c)::[]-> get_score c sc (*reached last node in layer*)
    | Node (sc, st, m, c) :: t -> 
      if State.current_player st = B then (*human's turn is next *)
        min (get_score c sc) (get_score t curr_score)
      else (*AI's turn is next*)
        max (get_score c sc) (get_score t curr_score) in

  (*[scored_children root_children] scores the second layer 
    (children of the root node) in the minmaxtree, which represents the immediate 
    possible next moves *)
  let scored_children root_children = List.map 
      (fun (Node (sc, st, m, c)) -> Node (get_score c sc, st, m, c)) 
      root_children in

  (**[extract_best_scoring_move scored_root_children] takes in the scored list
     of children produced by scored_schildren and returns the move with the best 
     score*)
  let extract_best_scoring_move scored_root_children =
    match (List.fold_left 
             (fun (best_sc, best_move) (Node (sc, st, move, c)) ->
                (if sc > best_sc then (sc, move)
                 else (best_sc, best_move))
             ) (-1000000, -1) scored_root_children) with
    | (sc, m)-> m in
  let Node (sc, state, move, children) = t in
  extract_best_scoring_move (scored_children (children))

(** [make_move_ai st] is a result [Legal st'] where [st'] is the state with 
    the new move determined by [eval_tree t]. *)
let make_move_ai (st:State.t) d : State.result = 
  State.go (eval_tree (generate_minmax_tree st d)) st
