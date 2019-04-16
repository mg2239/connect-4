(** The type for a game state. *)
type t

(** The type for the result of a move. *)
type result = Legal of t | Illegal

(** [init_state] is the initial state of the game. *)
val init_state : t

(** [board st] is the board of the game state [st] 
    @param st The game state
    @return The board of the game state *)
val board : t -> Board.t

(** [current_player st] is the current player of the game state [st]. 
    @param st The game state
    @return The current player of the game state *)
val current_player : t -> Board.color

(** [next_player st] is the next player of the game state [st].
    @param st The game state
    @return The next player of the game state *)
val next_player : t -> Board.color

(** [game_state st] is whether a player has won or not in game state [st]. 
    @param st The game state
    @return true if there exists a winner of color [c] in the game state.
            Otherwise false. *)
val game_state : t -> bool

(** [go b col st] is [r] if attempting to place a disk in column [col] of the 
    board in state [st] results in [r].
    @param col The column that the piece is being placed in 
    @param st The game state
    @return If [col] is not full, return [Legal st'], where [st'] is the board 
    with the new disk in [col] and the current player is the next player. 
    Otherwise return [Illegal]. *)
val go : int -> t -> result