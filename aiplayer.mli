(**
   Creates a AI that plays the game.
*)

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
    full. 
    Example: [generate_minmax_tree State.empty] is 
    Raises: None.
    @param st A game state.
    @return A minmax tree based on [st]. *)
val generate_minmax_tree : State.t -> minmaxtree

(** [eval_tree t] is the column number that the AI should play in, determined by
    evaluating a minmax tree [t].
    Example: [eval_tree t] where [t] is a tree with the most optimal move in
             column 1, is [1].
    Raises: None.
    @param t A minmax tree.
    @return A column number. *)
val eval_tree : minmaxtree -> int

(** [make_move_ai st] is a result [Legal st'] where [st'] is the state with 
    the new move determined by [eval_tree t].
    Example: [make_move_ai st] where the most optimal move in column 1, is
             [Legal st'] where [st'] is [st] with the AI's game piece in column
             1.
    Raises: None.
    @param st A game state.
    @return [Legal st'] where [st'] is the state with the new move. *)
val make_move_ai : State.t -> State.result