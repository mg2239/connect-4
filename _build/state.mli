(* The type for a game state. *)
type t

(* The type for the result of a move. *)
type result = Legal of t | Illegal

(** [init_state] is the initial state of the game. *)
val init_state : t

(** [current_player st] is the current player of the game state. *)
val current_player : t -> Board.color

(** [next_player st] is the next player of the game state. *)
val next_player : t -> Board.color

(** [game_state st] is whether a player has won or not. *)
val game_state : t -> bool

(** [go b col st] is [r] if attempting to place a disk in column [col] of the 
    board [b] in state [st] results in [r]. If [col] is not full, [r] is 
    [Legal st'], where [st'] is the board with the new disk and the current
    player is the next player. Otherwise [r] is [Illegal]. *)
val go : Board.t -> int -> t -> result