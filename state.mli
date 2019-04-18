(** 
   Representation of dynamic adventure state.
*)

(** The type for a game state. *)
type t

(** The type for the result of a move. *)
type result = Legal of t | Illegal

(** [init_state] is the initial state of the game. *)
val init_state : t

(** [board st] is the board of the game state [st].
    Example: [board {board: Board.empty; current: R}] is [Board.empty].
    Raises: None.
    @param st The game state.
    @return The board of the game state. *)
val board : t -> Board.t

(** [current_player st] is the current player of the game state [st].
    Example: [current_player {board: Board.empty; current: R}] is [R].
    Raises: None.
    @param st The game state.
    @return The current player of the game state. *)
val current_player : t -> Board.color

(** [next_player st] is the next player of the game state [st].
    Example: [next_player {board: Board.empty; current: R}] is [B].
    Raises: None.
    @param st The game state.
    @return The next player of the game state. *)
val next_player : t -> Board.color

(** [game_state st] is whether a player has gotten four in a row in game
    state [st].
    Example: [game_state {board: b; current: R}] where [b] contains a four in
             a row by [B] is [true].
    Raises: None.
    @param st The game state
    @return true if there exists a winner in the game state. Otherwise false. *)
val game_state : t -> bool

(** [go col st] is [r] if attempting to place a disk in column [col] of the 
    board in state [st] results in [r].
    Example: 
      [go 0 {board: Board.empty; current: R}] is [Legal {board: b; current: B}], 
      where [b] is the previous board but with a red game piece in column 0.
    Raises: None.
    @param col The column that the piece is being placed in 
    @param st The game state
    @return If [col] is not full, return [Legal st'], where [st'] is the board 
    with the new disk in [col] and the current player is the next player. 
    Otherwise return [Illegal]. *)
val go : int -> t -> result