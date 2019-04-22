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
  failwith ""

(** Takes fully evaluated tree with scores at Leafs (But not at nodes), 
    returns the next move for the ai to make *)
let eval_tree t = failwith ""

(** returns a state after the AI makes a move *)
let make_move_ai st = failwith ""